module Flavor (getAncestryFlavor, getBackgroundFlavor, getClassFlavor) where

import Types

getAncestryFlavor :: Ancestry -> String
getAncestryFlavor Dwarf =
  "If you want to play a character who is as hard as nails, "
    ++ "a stubborn and unrelenting adventurer, with a mix of "
    ++ "rugged toughness and deep wisdom—or at least dogged "
    ++ "conviction—you should play a dwarf."
getAncestryFlavor Elf =
  "If you want a character who is "
    ++ "magical, mystical, and mysterious, "
    ++ "you should play an elf."
getAncestryFlavor Gnome =
  "If you want a character with boundless enthusiasm "
    ++ "and an alien, fey outlook on morality and life, you should "
    ++ "play a gnome."
getAncestryFlavor Goblin =
  "If you want a character who is eccentric, enthusiastic, "
    ++ "and fun-loving, you should play a goblin."
getAncestryFlavor Halfling =
  "If you want to play a character who must contend with "
    ++ "these opposing drives toward adventure and comfort, "
    ++ "you should play a halfling."
getAncestryFlavor Human =
  "If you want a character who can be "
    ++ "just about anything, you should play a human."
getAncestryFlavor _ = "Coming soon.."

getBackgroundFlavor :: Background -> String
getBackgroundFlavor _ = "Coming soon"

getClassFlavor :: Class -> String
getClassFlavor Alchemist =
  "There’s no sight more beautiful to you than a strange brew bubbling in a beaker, "
    ++ "and you consume your ingenious elixirs with abandon. You’re fascinated by "
    ++ "uncovering the secrets of science and the natural world, and you’re constantly "
    ++ "experimenting in your lab or on the go with inventive concoctions for every eventuality. "
    ++ "You are fearless in the face of risk, hurling explosive or toxic creations at your foes. "
    ++ "Your unique path toward greatness is lined with alchemical brews that push your "
    ++ "mind and body to their limits."
getClassFlavor Barbarian =
  "Rage consumes you in battle. You delight in wreaking havoc and using powerful "
    ++ "weapons to carve through your enemies, relying on astonishing durability without "
    ++ "needing complicated techniques or rigid training. Your rages draw upon a vicious "
    ++ "instinct, which you might associate with an animal, a spirit, or some part of yourself. "
    ++ "To many barbarians, brute force is a hammer and every problem looks like a nail, "
    ++ "whereas others try to hold back the storm of emotions inside them and release "
    ++ "their rage only when it matters most."
getClassFlavor Bard =
  "You are a master of artistry, a scholar of hidden secrets, and a captivating "
    ++ "persuader. Using powerful performances, you influence minds and elevate "
    ++ "souls to new levels of heroics. You might use your powers to become a "
    ++ "charismatic leader, or perhaps you might instead be a counselor, manipulator, "
    ++ "scholar, scoundrel, or virtuoso. While your versatility leads some to consider "
    ++ "you a beguiling ne’er-do-well and a jack-of-all-trades, it’s dangerous to "
    ++ "dismiss you as a master of none."
getClassFlavor Champion =
  "You are an emissary of a deity, a devoted servant who has taken up a weighty "
    ++ "mantle, and you adhere to a code that holds you apart from those around "
    ++ "you. While champions exist for every alignment, as a champion of good, you "
    ++ "provide certainty and hope to the innocent. You have powerful defenses that "
    ++ "you share freely with your allies and innocent bystanders, as well as holy "
    ++ "power you use to end the threat of evil. Your devotion even attracts the "
    ++ "attention of holy spirits who aid you on your journey."
getClassFlavor Cleric =
  "Deities work their will upon the world in infinite ways, and you serve as one "
    ++ "of their most stalwart mortal servants. Blessed with divine magic, you live the "
    ++ "ideals of your faith, adorn yourself with the symbols of your church, and train "
    ++ "diligently to wield your deity’s favored weapon. Your spells might protect and "
    ++ "heal your allies, or they might punish foes and enemies of your faith, as your "
    ++ "deity wills. Yours is a life of devotion, spreading the teachings of your faith "
    ++ "through both word and deed."
getClassFlavor Druid =
  "The power of nature is impossible to resist. It can bring ruin to the stoutest "
    ++ "fortress in minutes, reducing even the mightiest works to rubble, burning them "
    ++ "to ash, burying them beneath an avalanche of snow, or drowning them beneath "
    ++ "the waves. It can provide endless bounty and breathtaking splendor to those "
    ++ "who respect it—and an agonizing death to those who take it too lightly. You "
    ++ "are one of those who hear nature’s call. You stand in awe of the majesty of its "
    ++ "power and give yourself over to its service."
getClassFlavor Fighter =
  "Fighting for honor, greed, loyalty, or simply the thrill of battle, you are an "
    ++ "undisputed master of weaponry and combat techniques. You combine your "
    ++ "actions through clever combinations of opening moves, finishing strikes, and "
    ++ "counterattacks whenever your foes are unwise enough to drop their guard. "
    ++ "Whether you are a knight, mercenary, sharpshooter, or blade master, you have "
    ++ "honed your martial skills into an art form and perform devastating critical "
    ++ "attacks on your enemies."
getClassFlavor Monk =
  "The strength of your fist flows from your mind and spirit. You seek "
    ++ "perfection—honing your body into a flawless instrument and your mind into "
    ++ "an orderly bastion of wisdom. You’re a fierce combatant renowned for martial "
    ++ "arts skills and combat stances that grant you unique fighting moves. While "
    ++ "the challenge of mastering many fighting styles drives you to great heights, "
    ++ "you also enjoy meditating on philosophical questions and discovering new ways "
    ++ "to obtain peace and enlightenment."
getClassFlavor Ranger =
  "Some rangers believe civilization wears down the soul, but still needs to be "
    ++ "protected from wild creatures. Others say nature needs to be protected from "
    ++ "the greedy, who wish to tame its beauty and plunder its treasures. You could "
    ++ "champion either goal, or both. You might be a scout, tracker, or hunter of "
    ++ "fugitives or beasts, haunting the edge of civilization or exploring the wilds. "
    ++ "You know how to live off the land and are skilled at spotting and taking down "
    ++ "both opportune prey and hated enemies."
getClassFlavor Rogue =
  "You are skilled and opportunistic. Using your sharp wits and quick reactions, "
    ++ "you take advantage of your opponents’ missteps and strike where it hurts "
    ++ "most. You play a dangerous game, seeking thrills and testing your skills, and "
    ++ "likely don’t care much for any laws that happen to get in your way. While "
    ++ "the path of every rogue is unique and riddled with danger, the one thing "
    ++ "you all share in common is the breadth and depth of your skills."
getClassFlavor Sorcerer =
  "You didn’t choose to become a spellcaster—you were born one. There’s magic "
    ++ "in your blood, whether a divinity touched one of your ancestors, a forebear "
    ++ "communed with a primal creature, or a powerful occult ritual influenced your "
    ++ "line. Self-reflection and study allow you to refine your inherent magical "
    ++ "skills and unlock new, more powerful abilities. The power in your blood "
    ++ "carries a risk, however, and you constantly face the choice of whether you’ll "
    ++ "rise to become a master spellcaster or fall into destruction."
getClassFlavor Wizard =
  "You are an eternal student of the arcane secrets of the universe, using your "
    ++ "mastery of magic to cast powerful and devastating spells. You treat magic "
    ++ "like a science, cross-referencing the latest texts on practical spellcraft "
    ++ "with ancient esoteric tomes to discover and understand how magic works. "
    ++ "Yet magical theory is vast, and there’s no way you can study it all. You "
    ++ "either specialize in one of the eight schools of magic, gaining deeper "
    ++ "understanding of the nuances of those spells above all others, or favor a "
    ++ "broader approach that emphasizes the way all magic comes together at the "
    ++ "expense of depth."
