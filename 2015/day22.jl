function parseinput(lines::Vector{String})
    return (parse(Int, match(r"\d+", line).match) for line in lines)
end

cache = Dict{Tuple{Bool, Int, Int, Int, Int, Int, Int, Bool}, Tuple{Bool, Int}}()

function simulate_battle(
    player_turn::Bool, boss_hp::Int, boss_damage::Int, player_hp::Int = 50, mana::Int = 500, shield_timer::Int = 0, poison_timer::Int = 0, recharge_timer::Int = 0;
    hard_mode::Bool = false
)
    key = (player_turn, boss_hp, player_hp, mana, shield_timer, poison_timer, recharge_timer, hard_mode)
    if haskey(cache, key)
        return cache[key]
    end
    if hard_mode && player_turn
        player_hp -= 1
    end
    if shield_timer > 0
        shield_timer -= 1
    end
    player_armor = shield_timer > 0 ? 7 : 0
    if poison_timer > 0
        boss_hp -= 3
        poison_timer -= 1
    end
    if recharge_timer > 0
        mana += 101
        recharge_timer -= 1
    end

    if player_hp ≤ 0
        return false, 0
    end
    if boss_hp ≤ 0
        return true, 0
    end

    if player_turn
        spell_outcomes = []
        # Magic Missile
        if mana ≥ 53
            is_win, mana_used = simulate_battle(false, boss_hp - 4, boss_damage, player_hp, mana - 53, shield_timer, poison_timer, recharge_timer, hard_mode=hard_mode)
            if is_win
                append!(spell_outcomes, 53 + mana_used)
            end
        end
        # Drain
        if mana ≥ 73
            is_win, mana_used = simulate_battle(false, boss_hp - 2, boss_damage, player_hp + 2, mana - 73, shield_timer, poison_timer, recharge_timer, hard_mode=hard_mode)
            if is_win
                append!(spell_outcomes, 73 + mana_used)
            end
        end
        # Shield
        if mana ≥ 113 && shield_timer == 0
            is_win, mana_used = simulate_battle(false, boss_hp, boss_damage, player_hp, mana - 113, 6, poison_timer, recharge_timer, hard_mode=hard_mode)
            if is_win
                append!(spell_outcomes, 113 + mana_used)
            end
        end
        # Poison
        if mana ≥ 173 && poison_timer == 0
            is_win, mana_used = simulate_battle(false, boss_hp, boss_damage, player_hp, mana - 173, shield_timer, 6, recharge_timer, hard_mode=hard_mode)
            if is_win
                append!(spell_outcomes, 173 + mana_used)
            end
        end
        # Recharge
        if mana ≥ 229 && recharge_timer == 0
            is_win, mana_used = simulate_battle(false, boss_hp, boss_damage, player_hp, mana - 229, shield_timer, poison_timer, 5, hard_mode=hard_mode)
            if is_win
                append!(spell_outcomes, 229 + mana_used)
            end
        end
        if length(spell_outcomes) == 0
            cache[key] = false, 53
            return false, 53
        else
            min_mana = min(spell_outcomes...)
            cache[key] = true, min_mana
            return true, min_mana
        end
    else
        player_hp -= max(1, boss_damage - player_armor)
        outcome = simulate_battle(true, boss_hp, boss_damage, player_hp, mana, shield_timer, poison_timer, recharge_timer, hard_mode=hard_mode)
        cache[key] = outcome
        return outcome
    end
end


boss_hp, boss_damage = parseinput(readlines())
println(simulate_battle(true, boss_hp, boss_damage)[2])
println(simulate_battle(true, boss_hp, boss_damage, hard_mode=true)[2])
