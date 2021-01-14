# c64-startrek
Cassette 50 charity competition entry

https://itch.io/jam/the-c64-cassette-50-charity-competition

Technical limitations:

 Final code must be in .PRG format that loads into memory below $1000 hex, giving 4,096 bytes of address space.
 At no point can the code write to any location at or above $1000, however using IO normally at $D000-$DFFF is allowed, as is calling BASIC or KERNAL routines. BASIC programs are welcome too, just be sure to start your program with POKE 56,16 so it conforms to the memory limits.

# STAR TREK: LAST HOPE

As captain of the Starfleet vessel USS Something you jump from DS9 into the wormhole to the Gamma Quadrant.

There you must fly to different planets and visit them with an away team to find relics: the plot.

Each sector that you visit can contain:
1. a friendly planet to visit (gain crew)
1. a hostile planet to visit (away team battle)
1. raiders (ship battle, losing crew and structural integrity in the process)
1. a space station to visit (heal integrity, upgrade ship?) (only do 1 option?)

You will have a limited crew, including some familiar sounding lead-characters: KIRK, JANE, JLUC, SISK, MIKA, etc.
Each character should have a different first character, so they can be depicted by a single characters.

Each away team follows a leader while being under attack from opposing forces: Klingon, Borg and others.

When your leader dies, the away team has lost (they will all be shot)

When all your lead-characters die, it's game over.

## Game mechanics

The game consists of a few +'s on screen representing the sector map. The USS Something will be there, as will
an object in each sector. Empty appearing sectors contain raiders that show up when you arrive.

Depending on the type of raiders the battle will be easy or hard.

The ship battle can occur visually between char-based ships that shoot sprites at each other.

Battle takes place in turns (debouncing joystick) with enemies only moving after you move.

You can choose your options via an on screen menu (that will be overlaid on the left when the ship is right
and vice versa).

When battling on the ground the game shows a single screen with a small background where you have to defeat
all the enemies. In the case of a friendly planet (you will know only when you teleport down) you can just
walk to a city.

Only hostile planets can have relics. Find all relics and fly back to DS9 to win the game.

When you lose a battle on a hostile planet, you cannot attack again immediately.

Raiders will re-emerge each time you enter the sector.

Visited friendly planets can no longer be visited.
