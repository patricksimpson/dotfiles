--
-- Load the information from the Alfred configuration.
--
require("alfred")

--
-- Place all your functions and configurations here. Running "hs:upgrade" will just
-- over right the alfred.lua file. DO NOT Change the alfred.lua file!
--

--
-- Turn off Animations.
--
hs.window.animationDuration = 0

choices =  {
   {
      ['text'] = 'VPN - Start',
      ['func'] = 'startVPN'
   },
   {
      ['text'] = 'VPN - Stop',
      ['func'] = 'stopVPN'
   },
}

function startVPN()
  os.execute('~/sh/anyconnect.sh')
  hs.alert.show('VPN Connected!')
end

function stopVPN()
  os.execute('/opt/cisco/anyconnect/bin/vpn disconnect')
  hs.alert.show('VPN Disonnected')
end


function chooser()
   local choose = hs.chooser.new(function(selection)
         if (selection) then
          _G[selection['func']]()
         end
   end)
   choose:choices(choices)
   choose:rows(3)
   choose:width(30)
   choose:show()
end

hs.hotkey.bind({"cmd, shift"}, ".", chooser)
