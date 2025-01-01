module Arkham.Campaigns.TheDunwichLegacy.FlavorText where

import Arkham.Prelude

import Arkham.Message

prologue :: FlavorText
prologue =
  FlavorText
    (Just "Prologue")
    [ "Dr. Henry Armitage pours himself a glass of pinot and sits down at his\
      \ desk, gesturing for you to sit across from him. “I apologize for the short\
      \ notice,” he begins. His face is pale, his forehead sweaty and wrinkled\
      \ with worry."
    , "Armitage—the head librarian of Miskatonic University, and a former\
      \ mentor of yours—privately contacted you in the hopes of gaining your\
      \ assistance. Eager to help, you made your way to his home in Southside.\
      \ Upon entering, you were surprised to find his home in disarray. Books\
      \ and notes litter his desk, and an empty bottle of wine has tipped over\
      \ onto the ground by the fireplace. You’d always known Armitage to be\
      \ neat and well-organized."
    , "The elderly man takes a moment to collect his thoughts. “I am\
      \ looking for two of my colleagues—Dr. Francis Morgan, professor of\
      \ archaeology, and Warren Rice, professor of languages. Warren was\
      \ supposed to meet up with me over supper earlier today to discuss several\
      \ important findings, but he has since gone missing. At first I thought\
      \ nothing of it, but I have a nagging feeling something else is going on. A\
      \ very…familiar feeling.” You’ve never seen Armitage quite this worried\
      \ before. His hands tremble as he reaches for the glass on his desk, and\
      \ he sips from it nervously. “I tried to find Francis, hoping he knew where\
      \ Warren was, but he too is out of touch. Francis has been spending a lot\
      \ of time in some gambling den, or so I am told."
    , "“I sent for you because I am worried Warren might be in trouble. I\
      \ would appreciate it greatly if you could find him for me. You may also\
      \ wish to ask Francis for help, if you can reach him.”"
    ]

armitagesFate1 :: FlavorText
armitagesFate1 =
  FlavorText
    (Just "Interlude I: Armitage's Fate")
    [ "You are more than a little rattled by your experiences\
      \ in the university and the Clover Club. You’re not sure what to make of\
      \ whoever—or whatever—was after Rice and Morgan. Worried about Dr.\
      \   Armitage, you swiftly make your way back to his home. When you arrive,\
      \ you find that the latches of his front door have been busted open, and his\
      \ living area and study have been ransacked. Dr. Armitage is nowhere to be\
      \ found. Searching his home, you find a journal the intruders didn’t steal\
      \ tucked beneath several other documents in the bottom drawer of Armitage’s\
      \ desk. The journal appears to be written in a strange language you cannot\
      \ decode, using a script you’ve never seen in your entire life. Fortunately, it\
      \ seems Dr. Armitage had already gone through the trouble of translating it\
      \ into English. Apparently, it belongs to one “Wilbur Whateley.”"
    , "The journal—along with Armitage’s many notes—tells a startling\
      \ tale, one you would scarcely believe had it not been for your harrowing\
      \ experiences earlier tonight…"
    ]

armitagesFate2 :: FlavorText
armitagesFate2 =
  FlavorText
    (Just "Interlude 1: Armitage's Fate")
    [ "When you arrive at Dr. Armitage’s home in\
      \ Southside, you find him sitting at his desk, pale-faced and sweating with\
      \ worry. He is grateful to you for searching for his colleagues, but he doesn’t\
      \ look relieved. With a long pause, he straightens his glasses and explains:"
    , "“I’m afraid I must apologize. There’s something I didn’t mention to you\
      \ earlier.” Dr. Armitage then spins a tale you would scarcely believe had it\
      \ not been for your harrowing experiences earlier that night…"
    ]

interlude2 :: FlavorText
interlude2 =
  FlavorText
    (Just "Interlude II: The Survivors")
    [ "Inside the chamber that contained the terrible beast, you find the missing\
      \ townsfolk and the others from Arkham; they are bound and shackled. You\
      \ also find several documents that suggest the creature you found isn’t the\
      \ only one of its kind in Dunwich. You free the creature’s victims from their\
      \ bonds, and they offer you their thanks. You begin to plan your next move."
    ]

interlude2DrHenryArmitage :: FlavorText
interlude2DrHenryArmitage =
  FlavorText
    (Just "Interlude II: The Survivors")
    [ "“It is far worse than we had thought,” Dr. Armitage\
      \ says, pale and trembling. “Wilbur Whateley was only the beginning.\
      \ There were more, many more in Dunwich, who knew of the ‘Great Old\
      \ Ones’ and who desired power and knowledge above all else, the Earth\
      \ be damned. I knew I should have burned that wretch’s journal. But\
      \ thanks to its contents, I know how we can stop them. We are the only\
      \ ones who can! Now quickly, help me with this solution—the powder is\
      \ the key, yes, the powder is the only way…”"
    ]

interlude2ProfessorWarrenRice :: FlavorText
interlude2ProfessorWarrenRice =
  FlavorText
    (Just "Interlude II: The Survivors")
    [ "Professor Rice adjusts his glasses and studies the\
      \ documents and arcane manuscripts left in the chamber. “I thought\
      \ this nightmare was over and done with,” he sighs. “But we have a duty\
      \ to see this through. We have to stop these creatures, or it won’t be just\
      \ Dunwich in trouble. The powder mixture Armitage created to see the\
      \ creatures will be our saving grace,” he explains, and sets off to the task\
      \ of recreating the powder."
    ]

interlude2DrFrancisMorgan :: FlavorText
interlude2DrFrancisMorgan =
  FlavorText
    (Just "Interlude II: The Survivors")
    [ "“Thank you for everything you’ve done,” Dr. Morgan\
      \ says, taking count of your provisions and ammunition. “Last time, we\
      \ needed some of that strange powder Armitage concocted to even see the\
      \ beast that terrorized Dunwich. If there’s more of those things out there,\
      \ we’re going to need that powder. I think I remember how he made it…”"
    ]

interlude2ZebulonWhateley :: FlavorText
interlude2ZebulonWhateley =
  FlavorText
    (Just "Interlude II: The Survivors")
    [ "“Dunwich’s had its fair share of oddities,” Zebulon\
      \ explains to you with a quavering voice, “but I ain’t ever seen anything\
      \ as sick and twisted as this…this…thing.” He gives the creature’s\
      \ remains one last sickened glance before closing the door to the chamber\
      \ behind him, shuddering. He locks eyes with you, his expression grim.\
      \ “Whoever dun this gotta pay. I’ll do all I can to help.”"
    ]

interlude2EarlSawyer :: FlavorText
interlude2EarlSawyer =
  FlavorText
    (Just "Interlude II: The Survivors")
    [ "“I never could’a made it if it weren’t for you,” Earl says with a stammer,\
      \ shaking your hand repeatedly. “If ’n there’s anything I can do to repay\
      \ yeh, just ask away. I ain’t much of a fighter or anythin’, but I’ll do all I\
      \ can. Jus’…don’t make me look at anythin’ like that beast again, a’right?”"
    ]

epilogue1 :: FlavorText
epilogue1 =
  FlavorText
    (Just "Epilogue")
    [ "Six months had passed, and life for Curtis Whateley was finally\
      \ returning to normal. Since they’d hushed up the incident and the\
      \ authorities kept their distance from these parts, Mr. Osborn had auctioned\
      \ off the Ericks and Bishop families’ belongings. Joe took a few trinkets for\
      \ himself, of course, but Curtis wanted nothing to do with it. Folk already\
      \ didn’t trust him on account of his family’s sins, even though he’d been\
      \ attending church on the regular and had stayed far away from anything\
      \ resembling the arcane."
    , "Even so, Curtis could not shake a sense of foreboding. He’d seen the\
      \ devastation that befell the Ericks, Bishop, and Frye homes. He’d even seen\
      \ firsthand the creature the folks from Arkham had banished. Some days, his\
      \ thoughts led him to Wilbur Whateley’s farm, where he would stand outside\
      \ and stare for hours, too scared to enter, but too curious to leave. Maybe\
      \ there was something in there that could make these terrible visions depart.\
      \ Something that could give him strength enough to resist the nightmares.\
      \ Curtis decided he would find it."
    ]

epilogue2 :: FlavorText
epilogue2 =
  FlavorText
    (Just "Epilogue")
    [ "Gené knelt closer to the ground to examine the sign in the dirt. The\
      \ shape of the sign was foreign to her, and the tracks that had led to it\
      \ were unlike any she’d ever seen. Coupled with the report from the\
      \ university...she wasn’t sure what to think. “Ms. Beauregard,” her assistant\
      \ called out to her, “have you found something?”"
    , "Gené stood and turned to face the freshman who’d been assigned to her.\
      \ He was a good kid and a bit callow, but a hard worker and eager to prove\
      \ himself. “This place was supposed to be cordoned off,” Gené responded.\
      \ “Nobody’s lived here for months. And yet, see for yourself: their primitive\
      \ rites continue to this day.”"
    , "The student approached and joined Gené in examining the sign, flipping\
      \ through the pages of his journal to reference its symbols. “What do you\
      \ think it means?” he asked. She shook her head. Outsiders rarely traveled to\
      \ Dunwich, and all the signposts leading to the village had been taken down.\
      \ But these days, even the townsfolk who had called this place home had\
      \ abandoned it."
    , "“I don’t know,” Gené admitted. “But somebody is here, and this isn’t the\
      \ first sign we’ve seen like this. Stories say there’s an altar where the\
      \ townsfolk would conduct pagan rituals atop that hill. Maybe we’ll find more\
      \ up there.”"
    ]
