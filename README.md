# c64-startrek
Cassette 50 charity competition entry

https://itch.io/jam/the-c64-cassette-50-charity-competition

Technical limitations:

 Final code must be in .PRG format that loads into memory below $1000 hex, giving 4,096 bytes of address space.
 At no point can the code write to any location at or above $1000, however using IO normally at $D000-$DFFF is allowed, as is calling BASIC or KERNAL routines. BASIC programs are welcome too, just be sure to start your program with POKE 56,16 so it conforms to the memory limits.

# STAR TREK: LAST HOPE

The USS Firebird has arrived at DS709, deep in the Gamma Quadrant. In this remote sector of the galaxy we hope to find the relics that can save our planet Earth from a mysterious disease that has already took millions of lives.

Star Fleet has gathered a crew of elite captains that lead away teams in search of the relics. But the sectors are filled with raiders so it will be a dangerous voyage.

We need you to guide us. You are our last hope.

Each sector that you visit can contain:
1. a hostile planet to visit (away team battle)
1. raiders (ship fight, losing structural integrity in the process)
1. a space station (heal integrity)

You will have a limited crew, including some familiar sounding lead-characters: KIRK, JEAN LUC, KATHERINE, ARCHER, SARU and MICHAEL. Each lead-character is depicted by the first letter of his/her name.

On a planet, the away team follows their leader exactly while trying to avoid sentinel lasers.

## Game mechanics

The game consists of a few +'s on screen representing the sector map. The USS Firebird will be there, as will an object in each sector. Empty appearing sectors contain raiders that show up when you arrive.

Depending on the type of raider the fight will be relatively easy or hard.

The ship fight occurs visually between PETSCII ships that shoot a torpedo sprite at each other.

The fight takes place in turns (debouncing joystick) with enemies only moving after you move.

You can choose your options via an on screen menu.

When battling on the planet the game shows a single screen with a small background where you have to avoid all the enemies. The relics are kept in a temple.
Lead the main character into the temple to win the relic.

Once you have gathered all relics, fly back to DS709 to win the game.

Raiders will re-emerge each time you enter the sector, also at planets.

## Implementation

The game is written in ACME 6502 assembler code in Visual Studio Code with the `vs64` extension. Just build with `acme -v -f cbm -o startrek.prg startrek.asm`.

It loads from `$0120` in memory and thus overwrites many BASIC and KERNAL variables.
It starts automatically by overwriting the stack with the start address.
This might not work on all C64 versions or emulators but it is sufficient for the competition. A small `loader.asm` is provided to create a version that loads and runs from `$0801` like a regular program (used for the lvllvl/c64 browser version.)

### Details

* The game is won when the ship reaches DS709 with 3 relics
* The game is lost when the ship runs out of hitpoints during a fight
* The game is also lost when you are out of captains when transporting down
* Most sectors have 80% chance of a small raider, a few on the left only 50%
* Each planet is guarded by a (returning) large raider
* At the start of the ship fight, your ship always gains shields
* Firing a torpedo will also increase your chance to hit next time
* Evasive maneuvers will lower the hit chance of the raider
* Fleeing will cause shields to drop and allows the raider a last shot
* The 3 stations completely restore all hitpoints of the ship
* On the planet, the away team is lost immediately if the captain is shot by the laser
* The away team is also lost when all other members are shot by the laser
* You gain a relic when the captain safely reaches the temple
* Each time you transport to the planet, the terrain is randomized and you control a different captain
* Each time you start the game, a random navigator is picked
