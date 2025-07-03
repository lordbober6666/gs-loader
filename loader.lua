local json = {
    decode = function(str)
        return loadstring("return " .. str)()
    end,
    encode = function(tbl)
        local result = {}
        for k, v in pairs(tbl) do
            table.insert(result, string.format('"%s":%s', k, type(v) == "string" and '"' .. v .. '"' or tostring(v)))
        end
        return "{" .. table.concat(result, ",") .. "}"
    end
}

local http = require("gamesense/http")
local json = json

-- Configuration
local GITHUB_USERNAME = "lordbober666"
local GITHUB_REPO = "gs-loader"
local GITHUB_BRANCH = "main" -- or "master"
local DISCORD_WEBHOOK_URL = "https://discord.com/api/webhooks/1390248976003108906/NBirdDwqBq-C9B4qk6MC0m2jXWCd4B4H7lgfk6i1mWRCHoH-ucwy3iZRT1PxivlRKlUq"
local scripts_list = {"quantum"} -- Your actual script names (without .lua)

-- HWID generation function (simple example)
local function get_hwid()
    local hwid = ""
    
    -- Combine several system identifiers to create a semi-unique HWID
    if user then
        hwid = hwid .. (user.name or "unknown")
    end
    if steam then
        hwid = hwid .. (steam.id or "unknown")
    end
    if system then
        hwid = hwid .. (system.hostname or "unknown")
    end
    
    -- Simple hash of the combined string
    local hash = 0
    for i = 1, #hwid do
        hash = (hash * 31 + hwid:byte(i)) % 0x7FFFFFFF
    end
    
    return tostring(hash)
end

-- Key verification function
local function verify_key(key, callback)
    local hwid = get_hwid()
    
    -- Send verification request to Discord webhook
    local payload = {
        content = string.format("Key verification request:\nKey: %s\nHWID: %s", key, hwid)
    }
    
    http.post(DISCORD_WEBHOOK_URL, {
        body = json.encode(payload),
        headers = {
            ["Content-Type"] = "application/json"
        }
    }, function(success, response)
        if not success then
            callback(false, "Failed to contact verification server")
            return
        end
        
        -- In a real implementation, you would have a server process the webhook
        -- and respond through another channel. This is a simplified version.
        -- You would typically check a database or have an admin respond.
        
        -- For demo purposes, we'll assume the key is valid if the webhook succeeds
        callback(true, "Key verified successfully")
    end)
end

-- Load script from GitHub
local function load_script_from_github(script_name, callback)
    local url = string.format("https://raw.githubusercontent.com/%s/%s/%s/%s.lua", 
        GITHUB_USERNAME, GITHUB_REPO, GITHUB_BRANCH, script_name)
    
    http.get(url, function(success, response)
        if not success or not response.body then
            callback(false, "Failed to fetch script from GitHub")
            return
        end
        
        -- Load and execute the script
        local chunk, err = load(response.body, script_name, "t")
        if not chunk then
            callback(false, "Failed to load script: " .. (err or "unknown error"))
            return
        end
        
        local success, err = pcall(chunk)
        if not success then
            callback(false, "Script error: " .. (err or "unknown error"))
            return
        end
        
        callback(true, "Script loaded successfully")
    end)
end

-- UI for key input and script selection
local key_input = ""
local selected_script = ""
local scripts_list = {"aimbot", "triggerbot", "esp", "visuals"} -- Your script names

local function draw_loader_ui()
    ui.text("Script Loader")
    ui.separator()
    
    -- Key input
    key_input = ui.text_input("Enter license key", key_input)
    
    -- Script selection
    selected_script = ui.listbox("Select script", scripts_list, selected_script)
    
    -- Load button
    if ui.button("Load Script") then
        if #key_input < 5 then
            client.notify("Error", "Please enter a valid key")
            return
        end
        
        if not selected_script or selected_script == "" then
            client.notify("Error", "Please select a script")
            return
        end
        
        client.notify("Info", "Verifying key...")
        
        verify_key(key_input, function(success, message)
            client.notify(success and "Success" or "Error", message)
            
            if success then
                client.notify("Info", "Loading script...")
                load_script_from_github(selected_script, function(load_success, load_message)
                    client.notify(load_success and "Success" or "Error", load_message)
                end)
            end
        end)
    end
    
    ui.text("HWID: " .. get_hwid())
end

-- Register the UI
client.set_event_callback("paint", function()
    if ui.is_menu_open() then
        draw_loader_ui()
    end
end)
