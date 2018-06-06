require 'yaml'
require 'pathname'
require 'optparse'

STACK       = "stack"
ENVIRONMENT = "env"
CODE        = "code"
DUMP        = "dump"
VMEXEC      = "exec"
FUNCPREFIX  = "get_"
RET         = "ret"
TRANSPRIM = "transform_primitive"

DESTRUCTURING_RULES = {
  "int" => lambda {|v| "IntegerConstant(#{v})"},
  "bool" => lambda {|v| "BooleanConstant(#{v})"},
  "context" => lambda {|v| "Context(#{v})"},
  "float" => lambda {|v| "FloatConstant(#{v})"},
  "horz" => lambda {|v| "Horz(#{v})"},
  "vert" => lambda {|v| "Vert(#{v})"},
  "length" => lambda {|v| "LengthConstant(#{v})"},
  "math" => lambda {|v| "MathValue(#{v})"},
  "path_value" => lambda {|v| "PathValue(#{v})"},
  "prepath" => lambda {|v| "PrePathValue(#{v})"},
  "regexp" => lambda {|v| "RegExpConstant(#{v})"},
}

def default_false b
  if b == nil then false else b end
end

def gen_vminstrs
  YAML.load_documents(ARGF.read) do |inst|
    tmpn = 0
    destruct = []
    funcapp = []

    inst["params"].each do |param|
      if param.is_a?(String) then
        destruct.push param
      elsif param.is_a?(Hash) then
        pat = param.keys[0]
        type = param.values[0]
        rule = DESTRUCTURING_RULES[type]
        if rule then
          destruct.push (rule.call pat)
        else
          destruct.push "_tmp#{tmpn}"
          funcapp.push [pat, type, "_tmp#{tmpn}"]
          tmpn = tmpn + 1
        end
      end
    end if inst["params"]

    if inst["fields"] != nil then
      fieldnames = inst["fields"].collect{|e| e.keys[0]}
      puts "  | Op#{inst["inst"]}(#{fieldnames.join ', '}) ->"
    else
      puts "  | Op#{inst["inst"]} ->"
    end
    puts "      begin"
    puts "        match #{STACK} with" if inst["params"]

    puts "        | #{destruct.reverse.join(" :: ")} :: #{STACK} ->" if destruct.length > 0

    funcapp.each do |app|
      dest, func, src = app
      puts "            let #{dest} = #{FUNCPREFIX}#{func} #{src} in"
    end
    if inst["is-primitive"] then
      puts "            let #{RET} ="
    else
      puts "            begin"
    end
    inst["code"].each_line do |line|
      puts "              #{line}"
    end
    if inst["is-primitive"] then
      puts "            in #{VMEXEC} (#{RET} :: #{STACK}) #{ENVIRONMENT} #{CODE} #{DUMP}"
    else
      puts "            end"
    end
    puts ""
    puts "        | _ -> report_bug_vm \"invalid argument for Op#{inst["inst"]}\"" if inst["params"]
    puts "      end"
    puts ""
  end
end

def gen_insttype
  puts "and instruction ="
  YAML.load_documents(ARGF.read) do |inst|
    if inst["fields"] != nil then
      fieldtypes = inst["fields"].collect{|e| e.values[0]}
      puts "  | Op#{inst["inst"]} of #{fieldtypes.join ' * '}"
    else
      puts "  | Op#{inst["inst"]}"
    end

    puts "      [@printer (fun fmt _ -> Format.fprintf fmt \"Op#{inst["inst"]}(...)\")]" if inst["suppress-pp"]
    puts "      [@printer (#{inst["custom-pp"]})]" if inst["custom-pp"]
  end
  puts "  [@@deriving show]"
end

def gen_attype
  YAML.load_documents(ARGF.read) do |inst|
    if inst["is-primitive"] && !default_false(inst["no-ircode"]) then
      if inst["params"] != nil then
        puts "  | #{inst["inst"]} of #{(["abstract_tree"] * inst["params"].length).join ' * '}"
      else
        puts "  | #{inst["inst"]}"
      end
    end
  end
end

def gen_ircases
  YAML.load_documents(ARGF.read) do |inst|
    if inst["is-primitive"] && !default_false(inst["no-ircode"]) then
      params = [*1..inst["params"].length].collect{|n| "p"+n.to_s}

      puts "    | #{inst["inst"]}(#{params.join ', '}) ->"
      puts "        #{TRANSPRIM} env [#{params.join '; '}] Op#{inst["inst"]}"
      puts ""
    end
  end
end

def pp_include
  srcpath = Pathname.new(ARGV[0])

  puts "(***********************************************************)"
  puts "(*                                                         *)"
  puts "(*                                                         *)"
  puts "(*                                                         *)"
  puts "(*                   AUTO-GENERATED FILE                   *)"
  puts "(*                      DO NOT MODIFY                      *)"
  puts "(*                                                         *)"
  puts "(*                                                         *)"
  puts "(*                                                         *)"
  puts "(***********************************************************)"

  ARGF.each_line do |line|
    match = line.match(/\(\*\*\*\*.*include *: *(.*) *\*\*\*\*\)/)
    if match then
       inspath = Pathname.new(match[1].strip)
       if inspath.absolute? then
         openpath = inspath
       else
         openpath = srcpath.dirname.join(inspath.basename)
       end

       File.open(openpath) do |file|
         file.each_line do |line|
           puts line
         end
       end
    else
      puts line
    end
  end
end


opt = OptionParser.new

func = nil

opt.on('--gen-vm') {|v| func = method(:gen_vminstrs) }
opt.on('--gen-ir') {|v| func = method(:gen_ircases) }
opt.on('--gen-insttype') {|v| func = method(:gen_insttype) }
opt.on('--gen-attype') {|v| func = method(:gen_attype) }
opt.on('--pp-include') {|v| func = method(:pp_include) }

opt.parse!(ARGV)

func.call
