#' Create a random name for an exercise
#'
#' Functions for writing exercises
#'
#' Create a random name using the <animal>-<verb>-<noun> pattern.
#'
#' @export
new_exercise_name <- function() {
  paste(
    sample(animal_words, 1),
    sample(verb_words, 1),
    sample(everyday_nouns, 1),
    sep = "-"
  )
}
#' @export
is_three_part_name <- function(name) {
  grepl("^.*-.*-.*$", name)
}

animal_words <- c(
  "ant", "aspen", "ash", "ape", "bat", "avoid", "arm",
  "bear", "bee", "bird", "boy", "beech",
  "buck", "birch", "bobcat", "brother",
  "camel", "cat", "cheetah", "chicken", "calf",
  "cow", "croc", "child", "crow", "calf",
  "deer", "dog", "dolphin", "duck", "doe", "daisy", "dingo",
  "eagle", "elm", "elephant", "elbow",
  "fish", "fly", "fox", "frog", "finch", "falcon", "fir", "fawn", "finger",
  "giraffe", "goat", "goldfish", "girl", "gator",
  "hamster",  "horse", "hyena",
  "jellyfish", "jaquar",
  "kangaroo", "kitten", "kid", "lamb", "lion", "lobster", "leg",
  "maple", "monkey", "mouse", "nephew", "niece", "octopus", "owl", "oak",
  "panda", "pig", "puppy",  "pine", "pollen", "pony",
  "rabbit", "rat", "rhinosaurus", "reptile",
  "seal", "shark", "sheep", "snail", "snake", "spider", "seahorse",
  "squirrel", "spruce", "seaweed", "sister",
  "tiger", "turtle", "titmouse", "toe",
  "walrus", "wolf", "zebra"
)

verb_words <- c(
  "beat", "become", "begin", "bend","bet", "bid", "bite", "bathe",
  "blow",  "break", "bring", "build", "burn", "buy",
  "catch", "chew", "choose", "close",
  "come",  "cost", "cut",
  "dig", "dive", "do", "draw", "dream",  "drive",  "drink",
  "eat", "engage", "enjoy",
  "fall", "feel", "fight", "form",
  "find", "fly", "forget", "forgive", "freeze",
  "get", "give", "go", "grow",
  "hang",  "have",  "hear", "hide", "hit",  "hold",  "hurt",
  "iron", "jump",
  "keep",  "know",
  "light", "lay",  "lead", "leave",  "lend",
  "let", "lie", "lose", "look", "love", "lick",
  "make", "mean",  "meet", "mute", "open",
  "pay","put", "pitch", "pack", "point", "pull",
  "read","ride", "ring","rise",  "run", "refer",
  "say", "sail", "see",  "sell","send",  "show",   "shut",  "sing",
  "serve",
  "sit", "sleep",  "speak", "spend", "stand", "swim", "sharpen", "seek",
  "take", "talk", "teach",
  "tear",  "tell", "think",  "throw", "toss", "trim", "tug", "type",
  "understand", "walk",  "wake",  "wear", "win", "write", "wish"
)

everyday_nouns <- c(
  "bed", "blanket", "boat", "book", "bottle", "bowl", "bulb", "bolt", "blouse",
  "candy", "canoe", "car", "chair", "clock", "closet", "coat", "cotton",
  "dish", "door", "drawer", "dress",
  "fork", "futon", "fridge", "glasses", "gloves",
  "hamper", "hat",
  "jacket", "kayak", "kitchen", "knife", "knob",
  "lamp", "linen", "laundry",
  "magnet", "map", "mug", "mattress",
  "oven", "painting", "pantry",
  "pants", "pen", "pencil", "piano", "plant", "plate", "pot", "pan", "plyer",
  "radio", "ring", "roof", "room", "rug", "stair",
  "saucer", "saw", "scarf", "sheet", "shelf", "ship", "shirt", "sofa", "screen",
  "shoe", "socks",  "spoon", "stove", "suitcase",
  "table", "tv", "teacup", "vase", "window")



