Stats = @NamedTuple{hp::Int, damage::Int, armor::Int}

function parseinput(lines::Vector{String})::Stats
    hp, damage, armor = [parse(Int, match(r"\d+", line).match) for line in lines]
    return (hp=hp, damage=damage, armor=armor)
end

function is_win(player_stats::Stats, boss_stats::Stats)
    player_dmg = max(1, player_stats.damage - boss_stats.armor)
    boss_dmg = max(1, boss_stats.damage - player_stats.armor)
    player_turns = ceil(player_stats.hp / boss_dmg)
    boss_turns = ceil(boss_stats.hp / player_dmg)
    return player_turns ≥ boss_turns
end

function make_characters()
    weapons = [8 => 4, 10 => 5, 25 => 6, 40 => 7, 74 => 8]
    armors = [0 => 0, 13 => 1, 31 => 2, 53 => 3, 75 => 4, 102 => 5]
    rings = [0 => (0, 0), 25 => (1, 0), 50 => (2, 0), 100 => (3, 0), 20 => (0, 1), 40 => (0, 2), 80 => (0, 3)]
    characters = Vector{Pair{Int, Stats}}()
    for weapon in weapons
        for armor in armors
            # no rings
            append!(characters, [weapon[1] + armor[1] => (hp=100, damage=weapon[2], armor=armor[2])])
            # two rings, including empty ring
            for (i, ring1) in enumerate(rings)
                for ring2 in rings[i+1:lastindex(rings)]
                    append!(characters, [weapon[1] + armor[1] + ring1[1] + ring2[1] => (
                            hp=100,
                            damage=weapon[2]+ring1[2][1]+ring2[2][1],
                            armor=armor[2]+ring1[2][2]+ring2[2][2],
                    )])
                end
            end
        end
    end
    return characters
end

function find_best(characters::Vector{Pair{Int, Stats}}, boss_stats::Stats)
    winners = filter(ch -> is_win(ch[2], boss_stats), characters)
    return min(winners...)[1]
end

function find_worst(characters::Vector{Pair{Int, Stats}}, boss_stats::Stats)
    winners = filter(ch -> !is_win(ch[2], boss_stats), characters)
    return max(winners...)[1]
end


boss_stats = parseinput(readlines())
characters = make_characters()
println(find_best(characters, boss_stats))
println(find_worst(characters, boss_stats))
