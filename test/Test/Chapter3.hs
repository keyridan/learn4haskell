module Test.Chapter3
    ( chapter3
    ) where

import Test.Hspec (Spec, describe, it, shouldBe)

import Chapter3


chapter3 :: Spec
chapter3 = describe "Chapter3" $ do
    chapter3normal
    chapter3Advanced

chapter3normal :: Spec
chapter3normal = describe "Chapter3Normal" $ do
    describe "Task2: fight" $ do
        it "fight when knight loses" $ fight (Knight 1 1 1) (Monster 2 2 2) `shouldBe` -1        
        it "fight when knight wins" $ fight (Knight 2 2 2) (Monster 1 1 1) `shouldBe` 3        
        it "fight when nobody wins" $ fight (Knight 5 1 2) (Monster 5 1 1) `shouldBe` 2
    describe "Task2: fightToDeath" $ do
        it "fight when warrior loses" $ AliveHero(Knight 1 1 1) `fightToDeath` (Monster 2 2 2) `shouldBe` DeadHero
        it "fight when warrior wins" $ AliveHero(Knight 2 2 2) `fightToDeath` (Monster 1 1 1) `shouldBe` (AliveHero (Knight 2 2 3)) 
        it "fight when warrior wins" $ AliveHero(Knight 500 1 2) `fightToDeath` (Monster 499 1 1) `shouldBe` (AliveHero (Knight 2 1 3)) 
        it "fight when monster attacks dead warrior" $ DeadHero `fightToDeath` (Monster 1 1 1) `shouldBe` DeadHero 
        it "fight when monster attacks dead warrior" $ AliveHero(Knight 0 500 2) `fightToDeath` (Monster 1 1 1) `shouldBe` DeadHero 
        it "fight when warrior has negative health" $ AliveHero(Knight (-1) 500 2) `fightToDeath` (Monster 1 1 1) `shouldBe` DeadHero 
    describe "Task4: recreateCastleWith" $ do
        it "recreateCastle when none" $ recreateCastleWith None "smallOne" `shouldBe` (Castle "smallOne")
        it "recreateCastle when small one exists" $ recreateCastleWith (Castle "smallOne") "biggerOne" `shouldBe` (Castle "biggerOne")
        it "recreateCastle when castle with walls exists" $ recreateCastleWith (CastleWithWalls "smallOne") "biggerOne" `shouldBe` (CastleWithWalls "biggerOne")
    describe "Task4: buildCastle" $ do
        it "buildCastle when None" $ buildCastle (City None Library []) "smallOne" `shouldBe` (City (Castle "smallOne") Library [])
    describe "Task4: buildHouse" $ do
        it "buildHouse" $ buildHouse (City None Library []) One `shouldBe` (City None Library [One])
    describe "Task4: countPeople" $ do
        it "countPeople" $ countPeople Four `shouldBe` 4
    describe "Task4: hasAtLeastNPeople" $ do
        it "hasAtLeastNPeople when it has with number of houses" $ (City None Library [Two]) `hasAtLeastNPeople` 1 `shouldBe` True
        it "hasAtLeastNPeople when it has less" $ cityWithLessPeople `hasAtLeastNPeople` 10  `shouldBe` False
        it "city with 10 people hasAtLeastNPeople 10 people" $ cityWith10People `hasAtLeastNPeople` 10 `shouldBe` True
    describe "Task4: buildWalls" $ do
        it "buildWalls when can build walls returns city with walls" $ buildWalls cityWith10People `shouldBe` cityWithWalls
        it "buildWalls when has less than 10 people returns the same city" $ buildWalls cityWithLessPeople `shouldBe` cityWithLessPeople
        it "buildWalls when has no castle returns the same city" $ buildWalls cityWith10PeopleButNoCastle `shouldBe` cityWith10PeopleButNoCastle
        it "buildWalls when has enough people but has walls returns the same city" $ buildWalls cityWithWalls `shouldBe` cityWithWalls
    describe "Task7: append" $ do
        it "append Gold" $ (Gold 1) `append` (Gold 2) `shouldBe` (Gold 3)
        it "append List" $ ([1, 2] :: [Int]) `append` ([3, 4] :: [Int]) `shouldBe` [1, 2, 3, 4]
        it "append Maybe" $ Just(Gold 1) `append` Just(Gold 2) `shouldBe` Just(Gold 3)
        it "append Maybe when first is Nothing" $ Nothing `append` Just(Gold 2) `shouldBe` Just(Gold 2)
        it "append Maybe when second is Nothing" $ Just(Gold 1) `append` Nothing `shouldBe` Just(Gold 1)
        it "append Maybe when Nothing" $ (Nothing :: Maybe Gold) `append` (Nothing  :: Maybe Gold) `shouldBe` Nothing
    describe "Task8: enumeration type" $ do
        it "nextDay" $ nextDay Tuesday `shouldBe` Wednesday
        it "nextDay maxBound" $ nextDay Sunday `shouldBe` Monday
        it "daysToParty" $ daysToParty Monday `shouldBe` 4
        it "Monday is not weekend" $ isWeekend Monday `shouldBe` False
        it "Sunday is weekend" $ isWeekend Sunday `shouldBe` True
        
cityWithLessPeople :: City
cityWithLessPeople = (City (Castle "smallOne") Library [Two, Three, Four])

cityWith10People :: City
cityWith10People = (City (Castle "smallOne") Library [One, Two, Three, Four])

cityWithWalls :: City
cityWithWalls = cityWith10People{cityCastle = CastleWithWalls "smallOne"}

cityWith10PeopleButNoCastle :: City
cityWith10PeopleButNoCastle = cityWith10People{cityCastle = None}

chapter3Advanced :: Spec
chapter3Advanced = describe "Chapter3Advanced" $ do
    describe "Task9: boss fight actions" $ do
        it "knight takes potion" $ perform drinkHugePotion knight knight `shouldBe` (knightWithLotsOfHealth, knight)
        it "knight attacks knight" $ perform KnightAttack knight knightWithLotsOfHealth `shouldBe` (knight, setHealth knightWithLotsOfHealth (Health(499)))
        it "knight kills knight" $ perform KnightAttack knight knight `shouldBe` (knight, DeadKnight)
        it "knight attacks monster" $ perform KnightAttack knight monsterWithLotOfHealth `shouldBe` (knight, setHealth monsterWithLotOfHealth (Health(498)))
        it "monster attacks knight" $ perform MonsterAttack monster knightWithLotsOfHealth `shouldBe` (monster, setHealth knightWithLotsOfHealth (Health(499)))
        it "monster kills knight" $ perform MonsterAttack monster knight `shouldBe` (monster, DeadKnight)
    describe "Task9: boss fight" $ do
        it "knight with potions wins" $ battle knight whoDrinksPotionAndAttack knightWithLotsOfHealth whoAttacks `shouldBe` (setHealth knight (Health(2)), DeadKnight)
        it "knight with spell wins" $ battle knight whoCastsSpellAndAttack knightWithLotsOfHealth whoAttacks `shouldBe` (knightWithLotsOfDefence, DeadKnight)
        it "knight with escaped monster" $ battle knight whoDrinksHugePotionAndAttack monster whoScared `shouldBe` (knightWithLotsOfHealth, EscapedMonster)
        it "knight against monster" $ battle knight whoAttacks monster whoAlwaysAttacks `shouldBe` (knight, DeadMonster)
        it "monster against knight" $ battle monster whoAlwaysAttacks knight whoAttacks `shouldBe` (monster, DeadKnight)
        it "monster runs from monster with lots of health" $ battle monster whoScared knightWithLotsOfHealth whoAttacks `shouldBe` (EscapedMonster, knightWithLotsOfHealth)
        it "monster dies from monster with lots of health" $ battle monster whoAlwaysAttacks monsterWithLotOfHealth whoAlwaysAttacks `shouldBe` (DeadMonster, setHealth (monsterWithLotOfHealth) (Health(498)))
        it "dead knight against monster" $ battle DeadKnight whoAttacks monster whoAlwaysAttacks `shouldBe` (DeadKnight, monster)
        it "dead monster against knight" $ battle DeadMonster whoAlwaysAttacks knight whoAttacks `shouldBe` (DeadMonster, knight)
        it "no battle if knight has no actions" $ battle knight ([]::[KnightAction]) monster whoAlwaysAttacks `shouldBe` (knight, monster)
        it "no battle if monster has no actions" $ battle knight whoAttacks monster ([]::[MonsterAction]) `shouldBe` (knight, monster)

knight :: Knight'
knight = AliveKnight (HolyKnight (Health(1)) (Attack(2)) (Defence(1)))

knightWithLotsOfHealth :: Knight'
knightWithLotsOfHealth = AliveKnight (HolyKnight (Health(500)) (Attack(2)) (Defence(1)))

knightWithLotsOfDefence :: Knight'
knightWithLotsOfDefence = AliveKnight (HolyKnight (Health(1)) (Attack(2)) (Defence(501)))

whoAttacks :: [KnightAction]
whoAttacks = [KnightAttack]

monster :: Monster'
monster = AliveMonster (BloodyMonster (Health(1)) (Attack(2)))

whoAlwaysAttacks :: [MonsterAction]
whoAlwaysAttacks = [MonsterAttack]

whoScared :: [MonsterAction]
whoScared = [Run]

monsterWithLotOfHealth :: Monster'
monsterWithLotOfHealth = AliveMonster (BloodyMonster (Health(500)) (Attack(2)))

drinkPotion :: KnightAction
drinkPotion = (DrinkPotion(Health(2)))

drinkHugePotion :: KnightAction
drinkHugePotion = (DrinkPotion(Health(499)))

castSpell :: KnightAction
castSpell = (CastSpell(Defence(1)))

whoDrinksPotionAndAttack :: [KnightAction]
whoDrinksPotionAndAttack = [drinkPotion, KnightAttack]

whoDrinksHugePotionAndAttack :: [KnightAction]
whoDrinksHugePotionAndAttack = [drinkHugePotion, KnightAttack]

whoCastsSpellAndAttack :: [KnightAction]
whoCastsSpellAndAttack = [castSpell, KnightAttack]