-- GameSense Lua Loader (Appears in LUA Tab)
local http = require("gamesense/http")
local database = require("gamesense/database") -- For persistent key storage

-- GitHub Configuration
local GITHUB_USERNAME = "lordbober6666"
local GITHUB_REPO = "gs-loader"
local GITHUB_BRANCH = "master" -- or "master"

-- Script List (what appears in the dropdown)
local scripts_list = {
    "quantum"
}

-- HWID System
local function get_hwid()
    local hwid = ""
    if user then hwid = hwid .. (user.name or "") end
    if steam then hwid = hwid .. (steam.id or "") end
    if system then hwid = hwid .. (system.hostname or "") end
    return string.lower(hwid:gsub("%W", ""))
end

-- Key System (stored persistently)
local license_key = database.read("loader_license_key") or ""

-- Load Script from GitHub
local function load_script(name)
    local url = string.format(
        "https://raw.githubusercontent.com/%s/%s/%s/%s.lua",
        GITHUB_USERNAME, GITHUB_REPO, GITHUB_BRANCH, name
    )
    
    http.get(url, function(success, response)
        if success and response.body then
            local chunk, err = load(response.body, name, "t")
            if chunk then
                pcall(chunk)
                client.notify("Script Loaded", name .. " loaded successfully!")
            else
                client.notify("Error", "Failed to load: " .. (err or "unknown"))
            end
        else
            client.notify("Error", "Failed to download " .. name)
        end
    end)
end

-- Register in LUA Tab
local loader_tab = ui.reference("LUA", "LUA", "Scripts")

-- Key Input
local key_input = ui.new_textbox("LUA", "LUA", "License Key", license_key)

-- Save Key When Changed
ui.set_callback(key_input, function()
    database.write("loader_license_key", ui.get(key_input))
end)

-- Script Selection Dropdown
local script_selector = ui.new_combobox("LUA", "LUA", "Select Script", scripts_list)

-- Load Button
local load_button = ui.new_button("LUA", "LUA", "Load Script", function()
    local selected_script = ui.get(script_selector)
    if selected_script and selected_script ~= "" then
        load_script(selected_script)
    else
        client.notify("Error", "Please select a script first!")
    end
end)

-- Display HWID (optional)
ui.new_label("LUA", "LUA", "HWID: " .. get_hwid())
