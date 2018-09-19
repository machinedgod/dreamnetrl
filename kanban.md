## Backlog

- Arch linux package
    > Make deployable arch linux package
- Map generation
    > Procedural generation for buildings, interiors, city planning, roads - but not objects themselves
- Multiple levels
    > Ability to load multiple map levels and switch between them using stairs
- Vertical slice
    > First 10-15 minutes of gameplay.
    * [ ] Start in apartment.  Should make sure to get wallet, it has the code to the apartment, ID card and credit encryption key. If using computer, there's email from Devin.
    * [ ] Get out, go to bar. Talk to Moe, find out that Devin was here but has left. There's Johnny and Sally, the newcomers. Delgado is sitting in the corner.
    * [ ] Go to Devin's. He's in the middle of something, but can spare few minutes. He gives contact for a small scale job and a contact for base equipment/weaponry.
    * [ ] Go back to apartment. Call contacts. Get equipment. Do a first job.
- RPG systems
    > Research/invent great RPG system for everything
    * [ ] Skills are already there, remember, totals matter to be Jack of all trades
    * [ ] XP gain in each category
    * [ ] Needs trainers/training to convert xp to skillpoints
    * [ ] Needs masters to spend skillpoints on certain skills
    * [ ] How do skills relate to skillchecks?
- AI
    > General AI in the game
    * [ ] Personalities
    * [ ] Dwarffortress-like preferences
    * [ ] AI's know what are they good at, and their personality decides whether they go with what they're good in, or try to learn new things
- See if possible to refactor GameStates into data family, with enum promoted to kind, and  data instances that only ever have one valid constructor (carrying all the data)
    > This would make it possible to create tighter functions that can only ever operate on a single valid state, not all the states.
- See if its possible to refactor almost ALL States into Readers. Especially GameM really doesn't HAVE TO store GameState at all, since it all flows one into another.

## Analysis(1)

- Commands UI/State
    > Adding a state for commanding the team, plus rendering of it
    * [ ] Categorical approach to commands
    * [ ] Command queue for NPCs/Teammates
    * [ ] Giving orders to Teammates
    * [ ] Teammates execute given commands one by one
    * [ ] Rendering of tactical/command interface
    * [ ] Commands are interpolation functions that are fed increments (turns) until end

## Breakdown

- Command queues for NPC's
- Command queue for player
- Issuing orders to player

## To Do(2)

- Teammates/player execute active commands
- Implement all commands
    * [ ] Move to
    * [ ] Attack point
    * [ ] Attack target (follows)
    * [ ] Change stance
    * [ ] Give/take stuff (better interface?)
    * [ ] Equipment (dwarf fortress/uniforms?)

## In progress(3)

- Categorical definition of commands and orders
- Tactical/command game state
    * [ ] Tactical game state
    * [ ] Rendering
- Commands are interpolation functions that take increments/turns
    * [ ] Turn -> Character -> Character
    * [ ] isComplete

## Testing/QA(1)


## Completed, ready to merge

- Command queues for Teammates
- Issuing orders to teammates

## Merged to master!

