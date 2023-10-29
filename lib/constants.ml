(*Constants********************************************************************)
module Constants = struct
  (*Custom Types*)
  type gamestate = Home | Active

  (*Screen Constants*)
  let screen_width = ref 1100
  let screen_height = ref 720

  (*Current Gamestate*)
  let state = ref Home
end

(*Utility Functions*)
