
let _ =
  afterLoadingHTML (fun () ->
    let inputArea  = getElementById "input-area" in
    let outputArea = getElementById "output-area" in
    let submitButton = getElementById "submit-button" in
  )
