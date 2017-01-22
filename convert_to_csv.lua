options = require("options")

-- Convert from table to CSV string
function toCSV (tt)
  local s = ""
-- ChM 23.02.2014: changed pairs to ipairs 
-- assumption is that fromCSV and toCSV maintain data as ordered array
  for _,p in ipairs(tt) do  
    s = s .. "," .. p
  end
  return string.sub(s, 2)      -- remove first comma
end

-- _G[varname]
--
fields = { "full_name", "type", "scope"}
-- print(toCSV(options.options))
header = ""

-- Used to escape "'s by toCSV
function escapeCSV (s)
  if string.find(s, '[,"]') then
    s = '"' .. string.gsub(s, '"', '""') .. '"'
  end
  return s
end


-- Convert from table to CSV string
function toCSV (tt)
  local s = ""
-- ChM 23.02.2014: changed pairs to ipairs 
-- assumption is that fromCSV and toCSV maintain data as ordered array
  for _,p in ipairs(tt) do  
    s = s .. "," .. escapeCSV(p)
  end
  return string.sub(s, 2)      -- remove first comma
end

-- dumpToCsv()
-- for _, name in ipairs(fields) do
-- 	header = header..","..name
-- end


-- header= header:sub(2, #header)
header = toCSV(fields)
print(header)

function dumpTable(opt)
	local line = ""
	for _, name in ipairs(fields) do
		local append = ""
		if name == "scope" then
			-- print("SCOPE")
			append = toCSV(opt[name])
		else
			append = opt[name]
		end
		-- print(opt[name])
		line = line..","..append
	end
	return line:sub(2,#line)
end
  for i,opt in ipairs(options.options) do  
    -- s = s .. "," .. p
	print(dumpTable(opt))
  end

