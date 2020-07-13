--[[

  Nether mod for minetest

  Copyright (C) 2013 PilzAdam

  Permission to use, copy, modify, and/or distribute this software for
  any purpose with or without fee is hereby granted, provided that the
  above copyright notice and this permission notice appear in all copies.

  THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL
  WARRANTIES WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED
  WARRANTIES OF MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR
  BE LIABLE FOR ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES
  OR ANY DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS,
  WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION,
  ARISING OUT OF OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS
  SOFTWARE.

]]--


-- Parameters

local NETHER_CEILING = nether.DEPTH_CEILING
local NETHER_FLOOR   = nether.DEPTH_FLOOR
local TCAVE = 0.6
local BLEND = 128


-- Stuff

local math_max, math_min, math_abs, math_floor = math.max, math.min, math.abs, math.floor -- avoid needing table lookups each time a common math function is invoked

if minetest.read_schematic == nil then
	-- Using biomes to create the Nether requires the ability for biomes to set "node_cave_liquid = air".
	-- This feature was introduced by paramat in b1b40fef1 on 2019-05-19, but we can't test for
	-- it directly. However b2065756c was merged a few months later (in 2019-08-14) and it is easy
	-- to directly test for - it adds minetest.read_schematic() - so we use this as a proxy-test
	-- for whether the Minetest engine is recent enough to have implemented node_cave_liquid=air
	error("This " .. nether.modname .. " mapgen requires Minetest v5.1 or greater, use mapgen_nobiomes.lua instead.", 0)
end

local function override_underground_biomes()
	-- https://forum.minetest.net/viewtopic.php?p=257522#p257522
	-- Q: Is there a way to override an already-registered biome so I can get it out of the
	--    way of my own underground biomes without disturbing the other biomes registered by
	--    default?
	-- A: No, all you can do is use a mod to clear all biomes then re-register the complete
	--    set but with your changes. It has been described as hacky but this is actually the
	--    official way to alter biomes, most mods and subgames would want to completely change
	--    all biomes anyway.
	--    To avoid the engine side of mapgen becoming overcomplex the approach is to require mods
	--    to do slightly more complex stuff in Lua.

	-- take a copy of all biomes, decorations, and ores. Regregistering a biome changes its ID, so
	-- any decorations or ores using the 'biomes' field must afterwards be cleared and re-registered.
	-- https://github.com/minetest/minetest/issues/9288
	local registered_biomes_copy      = {}
	local registered_decorations_copy = {}
	local registered_ores_copy        = {}

	for old_biome_key, old_biome_def in pairs(minetest.registered_biomes) do
	   registered_biomes_copy[old_biome_key] = old_biome_def
	end
	for old_decoration_key, old_decoration_def in pairs(minetest.registered_decorations) do
	   registered_decorations_copy[old_decoration_key] = old_decoration_def
	end
	for old_ore_key, old_ore_def in pairs(minetest.registered_ores) do
		registered_ores_copy[old_ore_key] = old_ore_def
	 end

	-- clear biomes, decorations, and ores
	minetest.clear_registered_decorations()
	minetest.clear_registered_ores()
	minetest.clear_registered_biomes()

	-- Restore biomes, adjusted to not overlap the Nether
	for biome_key, new_biome_def in pairs(registered_biomes_copy) do
		local biome_y_max, biome_y_min = tonumber(new_biome_def.y_max), tonumber(new_biome_def.y_min)

		if biome_y_max > NETHER_FLOOR and biome_y_min < NETHER_CEILING then
			-- This biome occupies some or all of the depth of the Nether, shift/crop it.
			local spaceOccupiedAbove = biome_y_max - NETHER_CEILING
			local spaceOccupiedBelow = NETHER_FLOOR - biome_y_min
			if spaceOccupiedAbove >= spaceOccupiedBelow or biome_y_min <= -30000 then
				-- place the biome above the Nether
				-- We also shift biomes which extend to the bottom of the map above the Nether, since they
				-- likely only extend that deep as a catch-all, and probably have a role nearer the surface.
				new_biome_def.y_min = NETHER_CEILING + 1
				new_biome_def.y_max = math_max(biome_y_max, NETHER_CEILING + 2)
			else
				-- shift the biome to below the Nether
				new_biome_def.y_max = NETHER_FLOOR - 1
				new_biome_def.y_min = math_min(biome_y_min, NETHER_CEILING - 2)
			end
		end
		minetest.register_biome(new_biome_def)
	end

	-- Restore biome decorations
	for decoration_key, new_decoration_def in pairs(registered_decorations_copy) do
	   minetest.register_decoration(new_decoration_def)
	end
	-- Restore biome ores
	for ore_key, new_ore_def in pairs(registered_ores_copy) do
		minetest.register_ore(new_ore_def)
	 end
 end

-- Shift any overlapping biomes out of the way before we create the Nether biomes
override_underground_biomes()

-- nether:native_mapgen is used to prevent ores and decorations being generated according
-- to landforms created by the native mapgen.
-- Ores and decorations can be registered against "nether:rack" instead, and the lua
-- on_generate() callback will carve the Nether with nether:rack before invoking
-- generate_decorations and generate_ores.
minetest.register_node("nether:native_mapgen", {})

minetest.register_biome({
	name = "nether_caverns",
	node_stone  = "nether:native_mapgen", -- nether:native_mapgen is used here to prevent the native mapgen from placing ores and decorations.
	node_filler = "nether:native_mapgen", -- The lua on_generate will transform nether:rack_native into nether:rack then decorate and add ores.
	node_dungeon = "nether:brick",
	node_dungeon_alt = "nether:brick_cracked",
	node_dungeon_stair = "stairs:stair_nether_brick",
	-- Setting node_cave_liquid to "air" avoids the need to filter lava and water out of the mapchunk and
	-- surrounding shell (overdraw nodes beyond the mapchunk).
	-- This feature was introduced by paramat in b1b40fef1 on 2019-05-19, and this mapgen.lua file should only
	-- be run if the Minetest version includes it. The earliest tag made after 2019-05-19 is 5.1.0 on 2019-10-13,
	-- however we shouldn't test version numbers. minetest.read_schematic() was added by b2065756c and merged in
	-- 2019-08-14 and is easy to test for, we don't use it but it should make a good proxy-test for whether the
	-- Minetest version is recent enough to have implemented node_cave_liquid=air
	node_cave_liquid = "air",
	y_max = NETHER_CEILING,
	y_min = NETHER_FLOOR,
	vertical_blend = 0,
	heat_point = 50,
	humidity_point = 50,
})


-- Ores and decorations

dofile(nether.path .. "/mapgen_decorations.lua")

minetest.register_ore({
	ore_type       = "scatter",
	ore            = "nether:glowstone",
	wherein        = "nether:rack",
	clust_scarcity = 11 * 11 * 11,
	clust_num_ores = 3,
	clust_size     = 2,
	y_max = NETHER_CEILING,
	y_min = NETHER_FLOOR,
})

minetest.register_ore({
	ore_type       = "scatter",
	ore            = "default:lava_source",
	wherein        = "nether:rack_deep",
	clust_scarcity = 36 * 36 * 36,
	clust_num_ores = 4,
	clust_size     = 2,
	y_max = NETHER_CEILING,
	y_min = NETHER_FLOOR,
})

minetest.register_ore({
	ore_type       = "scatter",
	ore            = "nether:lava_crust",
	wherein        = "nether:rack_deep",
	clust_scarcity = 16 * 16 * 16,
	clust_num_ores = 4,
	clust_size     = 2,
	y_max = NETHER_CEILING,
	y_min = NETHER_FLOOR,
})

minetest.register_ore({
	ore_type       = "scatter",
	ore            = "default:lava_source",
	wherein        = "nether:rack",
	clust_scarcity = 36 * 36 * 36,
	clust_num_ores = 4,
	clust_size     = 2,
	y_max = NETHER_CEILING,
	y_min = NETHER_FLOOR,
})

minetest.register_ore({
	ore_type        = "blob",
	ore             = "nether:sand",
	wherein         = "nether:rack",
	clust_scarcity  = 14 * 14 * 14,
	clust_size      = 8,
	y_max = NETHER_CEILING,
	y_min = NETHER_FLOOR
})


-- Mapgen

-- 3D noise

local np_cave = {
	offset = 0,
	scale = 1,
	spread = {x = 384, y = 128, z = 384}, -- squashed 3:1
	seed = 59033,
	octaves = 5,
	persist = 0.7,
	lacunarity = 2.0,
	--flags = ""
}

-- Buffers and objects we shouldn't recreate every on_generate

local nobj_cave = nil
local nbuf_cave = {}
local dbuf = {}

local yblmin = NETHER_FLOOR   + BLEND * 2
local yblmax = NETHER_CEILING - BLEND * 2

-- Content ids

local c_air              = minetest.get_content_id("air")
local c_netherrack       = minetest.get_content_id("nether:rack")
local c_netherrack_deep  = minetest.get_content_id("nether:rack_deep")
local c_dungeonbrick     = minetest.get_content_id("nether:brick")
local c_dungeonbrick_alt = minetest.get_content_id("nether:brick_cracked")
local c_netherbrick_slab = minetest.get_content_id("stairs:slab_nether_brick")
local c_netherfence      = minetest.get_content_id("nether:fence_nether_brick")
local c_glowstone        = minetest.get_content_id("nether:glowstone")
local c_lava_source      = minetest.get_content_id("default:lava_source")
local c_lavasea_source   = minetest.get_content_id("nether:lava_source") -- same as lava but with staggered animation to look better as an ocean
local c_lava_crust       = minetest.get_content_id("nether:lava_crust")
local c_basalt           = minetest.get_content_id("nether:basalt")
local c_native_mapgen    = minetest.get_content_id("nether:native_mapgen")


local c_debug = minetest.get_content_id("default:glass")


-- Dungeon excavation functions

function is_dungeon_brick(node_id)
	return node_id == c_dungeonbrick or node_id == c_dungeonbrick_alt
end

function build_dungeon_room_list(data, area)

	local result = {}

	-- Unfortunately gennotify only returns dungeon rooms, not corridors.
	-- We don't need to check for temples because only dungeons are generated in biomes
	-- that define their own dungeon nodes.
	local gennotify = minetest.get_mapgen_object("gennotify")
	local roomLocations = gennotify["dungeon"] or {}

	-- Excavation should still know to stop if a cave or corridor has removed the dungeon wall.
	-- See MapgenBasic::generateDungeons in mapgen.cpp for max room sizes.
	local maxRoomSize = 18
	local maxRoomRadius = math.ceil(maxRoomSize / 2)

	local xStride, yStride, zStride = 1, area.ystride, area.zstride
	local minEdge, maxEdge = area.MinEdge, area.MaxEdge

	for _, roomPos in ipairs(roomLocations) do

		if area:containsp(roomPos) then -- this safety check does not appear to be necessary, but lets make it explicit

			local room_vi = area:indexp(roomPos)
			--data[room_vi] = minetest.get_content_id("default:torch") -- debug

			local startPos = vector.new(roomPos)
			if roomPos.y + 1 <= maxEdge.y and data[room_vi + yStride] == c_air then
				-- The roomPos coords given by gennotify are at floor level, but whenever possible we
				-- want to be performing searches a node higher than floor level to avoids dungeon chests.
				startPos.y = startPos.y + 1
				room_vi = area:indexp(startPos)
			end

			local bound_min_x = math_max(minEdge.x, roomPos.x - maxRoomRadius)
			local bound_min_y = math_max(minEdge.y, roomPos.y - 1) -- room coords given by gennotify are on the floor
			local bound_min_z = math_max(minEdge.z, roomPos.z - maxRoomRadius)

			local bound_max_x = math_min(maxEdge.x, roomPos.x + maxRoomRadius)
			local bound_max_y = math_min(maxEdge.y, roomPos.y + maxRoomSize) -- room coords given by gennotify are on the floor
			local bound_max_z = math_min(maxEdge.z, roomPos.z + maxRoomRadius)

			local room_min = vector.new(startPos)
			local room_max = vector.new(startPos)

			local vi = room_vi
			while room_max.y < bound_max_y and data[vi + yStride] == c_air do
				room_max.y = room_max.y + 1
				vi = vi + yStride
			end

			vi = room_vi
			while room_min.y > bound_min_y and data[vi - yStride] == c_air do
				room_min.y = room_min.y - 1
				vi = vi - yStride
			end

			vi = room_vi
			while room_max.z < bound_max_z and data[vi + zStride] == c_air do
				room_max.z = room_max.z + 1
				vi = vi + zStride
			end

			vi = room_vi
			while room_min.z > bound_min_z and data[vi - zStride] == c_air do
				room_min.z = room_min.z - 1
				vi = vi - zStride
			end

			vi = room_vi
			while room_max.x < bound_max_x and data[vi + xStride] == c_air do
				room_max.x = room_max.x + 1
				vi = vi + xStride
			end

			vi = room_vi
			while room_min.x > bound_min_x and data[vi - xStride] == c_air do
				room_min.x = room_min.x - 1
				vi = vi - xStride
			end

			local roomInfo = vector.new(roomPos)
			roomInfo.minp = room_min
			roomInfo.maxp = room_max
			result[#result + 1] = roomInfo
		end
	end

	return result;
end

-- Only partially excavates dungeons, the rest is left as an exercise for the player ;)
-- (Corridors and the parts of rooms which extend beyond the emerge boundary will remain filled)
function excavate_dungeons(data, area, rooms)

	local vi, node_id

	-- any air from the native mapgen has been replaced by netherrack, but
	-- we don't want this inside dungeons, so fill dungeon rooms with air
	for _, roomInfo in ipairs(rooms) do

		local room_min = roomInfo.minp
		local room_max = roomInfo.maxp

		for z = room_min.z, room_max.z do
			for y = room_min.y, room_max.y do
				vi = area:index(room_min.x, y, z)
				for x = room_min.x, room_max.x do
					node_id = data[vi]
					if node_id == c_netherrack or node_id == c_netherrack_deep then data[vi] = c_air end
					vi = vi + 1
				end
			end
		end
	end
end

-- Since we already know where all the rooms and their walls are, and have all the nodes stored
-- in a voxelmanip already, we may as well add a little Nether flair to the dungeons found here.
function decorate_dungeons(data, area, rooms)

	local xStride, yStride, zStride = 1, area.ystride, area.zstride
	local minEdge, maxEdge = area.MinEdge, area.MaxEdge

	for _, roomInfo in ipairs(rooms) do

		local room_min, room_max = roomInfo.minp, roomInfo.maxp
		local room_size = vector.distance(room_min, room_max)

		if room_size > 10 then
			local room_seed = roomInfo.x + 3 * roomInfo.z + 13 * roomInfo.y
			local window_y  = roomInfo.y + math_min(2, room_max.y - roomInfo.y - 1)

			if room_seed % 3 == 0 and room_max.y < maxEdge.y then
				-- Glowstone chandelier (feel free to replace with a fancy schematic)
				local vi = area:index(roomInfo.x, room_max.y + 1, roomInfo.z)
				if is_dungeon_brick(data[vi]) then data[vi] = c_glowstone end

			elseif room_seed % 4 == 0 and room_min.y > minEdge.y
				   and room_min.x > minEdge.x and room_max.x < maxEdge.x
				   and room_min.z > minEdge.z and room_max.z < maxEdge.z then
				-- lava well (feel free to replace with a fancy schematic)
				local vi = area:index(roomInfo.x, room_min.y, roomInfo.z)
				if is_dungeon_brick(data[vi - yStride]) then
					data[vi - yStride] = c_lava_source
					if data[vi - zStride] == c_air then data[vi - zStride] = c_netherbrick_slab end
					if data[vi + zStride] == c_air then data[vi + zStride] = c_netherbrick_slab end
					if data[vi - xStride] == c_air then data[vi - xStride] = c_netherbrick_slab end
					if data[vi + xStride] == c_air then data[vi + xStride] = c_netherbrick_slab end
				end
			end

			-- Barred windows
			if room_seed % 7 < 5 and room_max.x - room_min.x >= 4 and room_max.z - room_min.z >= 4
			   and window_y >= minEdge.y and window_y + 1 <= maxEdge.y
			   and room_min.x > minEdge.x and room_max.x < maxEdge.x
			   and room_min.z > minEdge.z and room_max.z < maxEdge.z then
				--data[area:indexp(roomInfo)] = minetest.get_content_id("default:mese_post_light") -- debug

				-- Until whisper glass is added, every window will be made of netherbrick fence (rather
				-- than material depending on room_seed)
				local window_node = c_netherfence

				local vi_min = area:index(room_min.x - 1, window_y, roomInfo.z)
				local vi_max = area:index(room_max.x + 1, window_y, roomInfo.z)
				local locations = {-zStride, zStride, -zStride + yStride, zStride + yStride}
				for _, offset in ipairs(locations) do
					if is_dungeon_brick(data[vi_min + offset]) then data[vi_min + offset] = window_node end
					if is_dungeon_brick(data[vi_max + offset]) then data[vi_max + offset] = window_node end
				end
				vi_min = area:index(roomInfo.x, window_y, room_min.z - 1)
				vi_max = area:index(roomInfo.x, window_y, room_max.z + 1)
				locations = {-xStride, xStride, -xStride + yStride, xStride + yStride}
				for _, offset in ipairs(locations) do
					if is_dungeon_brick(data[vi_min + offset]) then data[vi_min + offset] = window_node end
					if is_dungeon_brick(data[vi_max + offset]) then data[vi_max + offset] = window_node end
				end
			end

			-- Weeds on the floor once Nether weeds are added
		end
	end
end


local REGION_BUFFER_THICKNESS = 0.2
local CENTER_REGION_LIMIT     = TCAVE - REGION_BUFFER_THICKNESS
local BASALT_LIMIT            = CENTER_REGION_LIMIT - 0.1
local SURFACE_CRUST_LIMIT     = BASALT_LIMIT * 0.25
local CRUST_LIMIT             = BASALT_LIMIT * 0.85

-- Returns (absolute height, fractional distance from ceiling or sea floor)
-- the fractional distance from ceiling or sea floor is a value between 0 and 1 (inclusive)
-- Note it may find the most relevent sea-level - not necesssarily the one you are closest
-- to, since the space above the sea reaches much higher than the depth below the sea.
local function find_nearest_lava_sealevel(y)
	-- todo: put oceans near the bottom of chunks to improve ability to generate tunnels to the center
	-- todo: constrain y to be not near the bounds of the nether
	-- todo: add some random adj at each level, seeded only by the level height
	local sealevel = math.floor((y + 100) / 200) * 200

	local cavern_limits_fraction
	local height_above_sea = y - sealevel
	if height_above_sea >= 0 then
		cavern_limits_fraction = math_min(1, height_above_sea / 95)
	else
		-- approaches 1 much faster as the lava sea is shallower than the cavern above it
		cavern_limits_fraction = math_min(1, -height_above_sea / 40)
	end

	return sealevel, cavern_limits_fraction
end



local caveperlin = nil
minetest.register_chatcommand("nether_whereami",
    {
		description = "Describes which region of the nether the player is in",
		privs = {debug = true},
		func = function(name, param)

			local player = minetest.get_player_by_name(name)
			if player == nil then return false, "Unknown player position" end

			local pos = vector.round(player:get_pos())
			if pos.y > NETHER_CEILING or pos.y < NETHER_FLOOR then
				return true, "The Overworld"
			end

			caveperlin = caveperlin or minetest.get_perlin(np_cave)
			local densityNoise = caveperlin:get_3d(pos)
			local sea_level, cavern_limit_distance = find_nearest_lava_sealevel(pos.y)
			local desc

			if densityNoise > 0.6 then
				desc = "Positive nether"
			elseif densityNoise < -0.6 then
				desc = "Negative nether"
			elseif math_abs(densityNoise) < CENTER_REGION_LIMIT then
				desc =  "Nether Core"

				local cavern_noise_adj = CENTER_REGION_LIMIT * (cavern_limit_distance * cavern_limit_distance * cavern_limit_distance)
				if math_abs(densityNoise) + cavern_noise_adj < BASALT_LIMIT then
					desc =  desc .. " inside cavern"
				else
					desc =  desc .. " but outside cavern"
				end

			elseif densityNoise > 0 then
				desc =  "Shell between positive nether and center region"
			else
				desc = "Shell between negative nether and center region"
			end

			local sea_pos = pos.y - sea_level
			if sea_pos > 0 then
				desc = desc .. ", " .. sea_pos .. "m above lava-sea level"
			else
				desc = desc .. ", " .. sea_pos .. "m below lava-sea level"
			end

			return true, "[Perlin " .. (math_floor(densityNoise * 1000) / 1000) .. "] " .. desc
		end
	}
)

function draw_pathway(data, area, nether_pos, center_pos)


	local dist = math_floor(vector.distance(nether_pos, center_pos))

	local step = vector.subtract(center_pos, nether_pos)
	local ystride = area.ystride
	local zstride = area.zstride

	-- first pass: record path details
	local linedata = {}
	local last_pos = {}
	local line_index = 1
	local first_filled_index, boundary_index, last_filled_index
	for i = 0, dist do
		-- bresenham's line would be good here, but too much lua code
		local pos = vector.round(vector.add(nether_pos, vector.multiply(step, i / dist)))
		if not vector.equals(pos, last_pos) then
			local vi = area:indexp(pos)
			local node_id = data[vi]
			linedata[line_index] = {
				pos = pos,
				vi = vi,
				node_id = node_id
			}
			if boundary_index == nil and node_id == c_netherrack_deep then
				boundary_index = line_index
			end
			if node_id == c_air then
				if boundary_index ~= nil and last_filled_index == nil then
					last_filled_index = line_index
				end
			else
				if first_filled_index == nil then
					first_filled_index = line_index
				end
			end
			line_index = line_index + 1
			last_pos = pos
		end
	end
	first_filled_index = first_filled_index or 1
	last_filled_index  = last_filled_index  or #linedata
	boundary_index     = boundary_index     or last_filled_index

	--minetest.chat_send_all((last_filled_index - first_filled_index) .. " instead of " .. dist .. ", with steps reduced to " ..  #linedata)

	-- second pass: excavate
	-- excavation radius should be limited to half the sampling distance so we don't end up
	-- exceeding minp-maxp and having excavation filled in when the next chunk is generated.
	local start_index, stop_index = math_max(1, first_filled_index - 4), math_min(#linedata, last_filled_index + 4)
	for i = start_index, stop_index do
		local vi = linedata[i].vi
		local radius_squared = 26.5 - 3 * (i / (stop_index - start_index + 1))
		for x = -5, 5 do
			for y = -5, 5 do
				if x * x + y * y < radius_squared then
					data[vi + y * ystride + x]           = c_air
					data[vi + y * ystride + x * zstride] = c_air
					data[vi + y * zstride + x]           = c_air
				end
			end
		end
	end

	-- thrid pass: decorate
	--for i = start_index, stop_index do data[linedata[i].vi] = c_glowstone end

	-- add glowstone to make tunnels easyier to find
	local vi = linedata[boundary_index].vi
	local glowcount = 0
	for x = -6, 6 do
		for y = -6, 6 do
			if glowcount > 3 then break end
			local radius_squared = x * x + y * y
			if radius_squared < 27 and radius_squared >= 25 then
				if data[vi + y * ystride + x]           ~= c_air then data[vi + y * ystride + x]           = c_glowstone glowcount = glowcount + 1 end
				if data[vi + y * ystride + x * zstride] ~= c_air then data[vi + y * ystride + x * zstride] = c_glowstone glowcount = glowcount + 1 end
				if data[vi + y * zstride + x]           ~= c_air then data[vi + y * zstride + x]           = c_glowstone glowcount = glowcount + 1 end
			end
		end
	end



end

function excavate_tunnel_to_center_of_the_nether(data, area, nvals_cave, minp, maxp)

	local extent = vector.subtract(maxp, minp)
	local skip = 10 -- sampling rate of 1 in 10

	local highest = -1000
	local lowest  =  1000
	local lowest_vi
	local highest_vi

	local yCaveStride = maxp.x - minp.x + 1
	local zCaveStride = yCaveStride * yCaveStride

	local vi_offset = area:indexp(vector.add(minp, math_floor(skip / 2))) -- start half the sampling distance away from minp
	local vi, ni

	for y = 0, extent.y - 1, skip do
		local sealevel = find_nearest_lava_sealevel(minp.y + y)

		if minp.y + y > sealevel then -- only create tunnels above sea level
			for z = 0, extent.z - 1, skip do

				vi = vi_offset + y * area.ystride + z * area.zstride + 1
				ni = z * zCaveStride + y * yCaveStride + 1
				for x = 0, extent.x - 1, skip do

					local noise = math_abs(nvals_cave[ni])
					if noise < lowest then
						lowest = noise
						lowest_vi = vi
					end
					if noise > highest then
						highest = noise
						highest_vi = vi
					end
					ni = ni + skip
					vi = vi + skip
				end
			end
		end
	end

	if lowest < BASALT_LIMIT and highest > TCAVE + 0.03 then

		local sealevel, cavern_limit_distance = find_nearest_lava_sealevel(area:position(lowest_vi).y)
		local cavern_noise_adj = CENTER_REGION_LIMIT * (cavern_limit_distance * cavern_limit_distance * cavern_limit_distance)
		if lowest + cavern_noise_adj < BASALT_LIMIT then
			draw_pathway(data, area, area:position(highest_vi), area:position(lowest_vi))
		end
	end
end



local pathway_chunk_count = 0
local total_chunk_count = 0

-- On-generated function

local function on_generated(minp, maxp, seed)

	if minp.y > NETHER_CEILING or maxp.y < NETHER_FLOOR then
		return
	end

	local vm, emerge_min, emerge_max = minetest.get_mapgen_object("voxelmanip")
	local area = VoxelArea:new{MinEdge=emerge_min, MaxEdge=emerge_max}
	local data = vm:get_data(dbuf)

	local x0, y0, z0 = minp.x, math_max(minp.y, NETHER_FLOOR),   minp.z
	local x1, y1, z1 = maxp.x, math_min(maxp.y, NETHER_CEILING), maxp.z

	local yCaveStride = x1 - x0 + 1
	local zCaveStride = yCaveStride * yCaveStride
	local chulens = {x = yCaveStride, y = yCaveStride, z = yCaveStride}

	nobj_cave = nobj_cave or minetest.get_perlin_map(np_cave, chulens)
	local nvals_cave = nobj_cave:get_3d_map_flat(minp, nbuf_cave)


	local dungeonRooms = build_dungeon_room_list(data, area)
	local abs_cave_noise, abs_cave_noise_adjusted

	local contains_nether = false
	local contains_shell  = false
	local contains_center = false
	local contains_ocean = false


	for y = y0, y1 do -- Y loop first to minimise tcave & lava-sea calculations

		local tcave = TCAVE
		if y > yblmax then tcave = TCAVE + ((y - yblmax) / BLEND) ^ 2 end
		if y < yblmin then tcave = TCAVE + ((yblmin - y) / BLEND) ^ 2 end

		local sea_level, cavern_limit_distance = find_nearest_lava_sealevel(y)
		local above_lavasea = y > sea_level
		local below_lavasea = y < sea_level
		local cavern_noise_adj = CENTER_REGION_LIMIT * (cavern_limit_distance * cavern_limit_distance * cavern_limit_distance)


		for z = z0, z1 do
			local vi = area:index(x0, y, z) -- Initial voxelmanip index
			local ni = (z - z0) * zCaveStride + (y - y0) * yCaveStride + 1

			for x = x0, x1 do

				local id = data[vi] -- Existing node

				local cave_noise = nvals_cave[ni]

				if cave_noise > tcave then
					-- prime region
					data[vi] = c_air
					contains_nether = true

				elseif -cave_noise > tcave then
					-- secondary/spare region
					data[vi] = c_air

				else
					-- netherrack walls and/or center region
					abs_cave_noise = math_abs(cave_noise)

					-- abs_cave_noise_adjusted makes the center region smaller as distance from the lava ocean
					-- increases, we do this by pretending the abs_cave_noise value is higher.
					abs_cave_noise_adjusted = abs_cave_noise + cavern_noise_adj

					if above_lavasea and abs_cave_noise_adjusted < BASALT_LIMIT then
						data[vi] = c_air
						contains_center = true
					elseif abs_cave_noise_adjusted < SURFACE_CRUST_LIMIT or (below_lavasea and abs_cave_noise_adjusted < CRUST_LIMIT) then
						data[vi] = c_lavasea_source
						contains_ocean = true
					elseif abs_cave_noise_adjusted < BASALT_LIMIT then
						data[vi] = c_lava_crust
						contains_ocean = true
					elseif id == c_air or id == c_native_mapgen then

						if abs_cave_noise < CENTER_REGION_LIMIT then
							data[vi] = c_netherrack_deep
						else
							-- the shell seperating the basalt realm from the rest of the nether...
							-- put some holes in it
							data[vi] = c_netherrack -- excavate_dungeons() will mostly reverse this inside dungeons
							contains_shell = true
						end
					end
				end

				ni = ni + 1
				vi = vi + 1
			end
		end
	end

	total_chunk_count = total_chunk_count + 1
	if contains_nether and contains_shell and contains_center and contains_ocean then
		pathway_chunk_count = pathway_chunk_count + 1
	end
	if total_chunk_count % 50 == 0 then
		minetest.chat_send_all(pathway_chunk_count .. " of " .. total_chunk_count .. " chunks contain both nether and lava-sea (" .. math_floor(pathway_chunk_count * 100 / total_chunk_count) .. "%)")
	end

	if contains_nether and contains_center then
		excavate_tunnel_to_center_of_the_nether(data, area, nvals_cave, minp, maxp)
	end

	-- any air from the native mapgen has been replaced by netherrack, but we
	-- don't want netherrack inside dungeons, so fill known dungeon rooms with air.
	excavate_dungeons(data, area, dungeonRooms)
	decorate_dungeons(data, area, dungeonRooms)

	vm:set_data(data)

	minetest.generate_ores(vm)
	-- avoid generating decorations on the underside of the bottom of the nether
	if minp.y > NETHER_FLOOR and maxp.y < NETHER_CEILING then minetest.generate_decorations(vm) end

	vm:set_lighting({day = 0, night = 0}, minp, maxp)
	vm:calc_lighting()
	vm:update_liquids()
	vm:write_to_map()
end


-- use knowledge of the nether mapgen algorithm to return a suitable ground level for placing a portal.
-- player_name is optional, allowing a player to spawn a remote portal in their own protected areas.
function nether.find_nether_ground_y(target_x, target_z, start_y, player_name)
	local nobj_cave_point = minetest.get_perlin(np_cave)
	local air = 0 -- Consecutive air nodes found

	local minp_schem, maxp_schem = nether.get_schematic_volume({x = target_x, y = 0, z = target_z}, nil, "nether_portal")
	local minp = {x = minp_schem.x, y = 0, z = minp_schem.z}
	local maxp = {x = maxp_schem.x, y = 0, z = maxp_schem.z}

	for y = start_y, math_max(NETHER_FLOOR + BLEND, start_y - 4096), -1 do
		local nval_cave = nobj_cave_point:get3d({x = target_x, y = y, z = target_z})

		if nval_cave > TCAVE then -- Cavern
			air = air + 1
		else -- Not cavern, check if 4 nodes of space above
			if air >= 4 then
				local portal_y = y + 1
				-- Check volume for non-natural nodes
				minp.y = minp_schem.y + portal_y
				maxp.y = maxp_schem.y + portal_y
				if nether.volume_is_natural_and_unprotected(minp, maxp, player_name) then
					return portal_y
				else -- Restart search a little lower
					nether.find_nether_ground_y(target_x, target_z, y - 16, player_name)
				end
			else -- Not enough space, reset air to zero
				air = 0
			end
		end
	end

	return math_max(start_y, NETHER_FLOOR + BLEND) -- Fallback
end

-- We don't need to be gen-notified of temples because only dungeons will be generated
-- if a biome defines the dungeon nodes
minetest.set_gen_notify({dungeon = true})

minetest.register_on_generated(on_generated)