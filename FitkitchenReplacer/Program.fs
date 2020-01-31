open System
open System.Text
open HtmlAgilityPack

type Meal = string
type DayMenu = Meal seq
type WeekMenu = {
    Monday: DayMenu
    Tuesday: DayMenu
    Wednesday: DayMenu
    Thursday: DayMenu
    Friday: DayMenu
    Saturday: DayMenu
}
type Substitute = {
    OldMeal: Meal
    NewMeal: Meal
}

let favoriteMeals = Set.ofList [
    "Fitness burrito s kuřetem a rukolovým pestem a ricottou";
    "Salát se žlutým melounem a kozím sýrem, zálivka s dijonskou hořčicí a medem";
    "Čokoládové brownies s višněmi";
    "Hovězí maso v rajské omáčce, rýže"
]

let neutralMeals = Set.ofList [
    "Bílková omeleta s kaší s pohankových vloček";
    "Kukuřičná kaše s paštikou"
]

let badMeals = Set.ofList [
    "Rybí karbanátky z tresky, bramborovo-mrkvové pyré"
];

let getNextWeekMenu () =
    let html = HtmlWeb().Load(Uri("https://fitkitchen.cz/"))
    let weekParity = "even"
    let weekDays = html.DocumentNode.SelectNodes("//div[@class='menu_tabs']/ul[@class='tabs_content']/li[4]/div[@class='week_tabs']/ul[@class='tabs_content']/li/div[@class='slick " + weekParity + "']")
    let dayMenus =
        weekDays
        |> Seq.map (fun n ->
            n.DescendantsAndSelf()
            |> Seq.where (fun subNode -> subNode.GetAttributeValue("class", "undefined") = "name")
            |> Seq.map (fun subNode -> subNode.InnerText.Trim())
        )
        |> Seq.toArray
        
    {
        Monday = dayMenus.[0]
        Tuesday = dayMenus.[1]
        Wednesday = dayMenus.[2]
        Thursday = dayMenus.[3]
        Friday = dayMenus.[4]
        Saturday = dayMenus.[5]
    }
    
let getSubstitutes meals =
    let badMeals = meals |> Seq.where (fun m -> Set.contains m badMeals)
    let orderedBetterMeals =
        Seq.except badMeals meals
        |> Seq.sortBy (fun m -> if Set.contains m favoriteMeals then 0 else 1)
        |> Seq.toArray
    let betterMealsCount = Array.length orderedBetterMeals
    Seq.mapi (fun index badMeal -> { OldMeal = badMeal; NewMeal = orderedBetterMeals.[index % betterMealsCount] }) badMeals

let appendSubstitutes (sb: StringBuilder) days substitutes =
    if (Seq.length substitutes > 0) then
        sb.AppendLine("V rámci " + days + " poprosím:") |> ignore
        for substitute in substitutes do
            sb.AppendLine("Vyměnit '" + substitute.OldMeal + "' za '" +  substitute.NewMeal + "'") |> ignore
        sb.AppendLine() |> ignore
        
let generateSms monTueSubstitutes wedThruSubstitutes friSatSubstitutes =
    let sb = StringBuilder()
    sb.AppendLine("Dobrý den, na příští týden poprosím následující změny:") |> ignore
    appendSubstitutes sb "pondělí a úterý" monTueSubstitutes
    appendSubstitutes sb "středy a čtvrtku" wedThruSubstitutes
    appendSubstitutes sb "pátku a soboty" friSatSubstitutes
    sb.AppendLine("Předem děkuji,") |> ignore
    sb.AppendLine("Starýchfojtů") |> ignore
    sb.ToString()

[<EntryPoint>]
let main argv =
    let weekMenu = getNextWeekMenu ()
    let monTueSubstitutes = getSubstitutes (Seq.append weekMenu.Monday weekMenu.Tuesday)
    let wedThruSubstitutes = getSubstitutes (Seq.append weekMenu.Wednesday weekMenu.Thursday)
    let friSatSubstitutes = getSubstitutes (Seq.append weekMenu.Friday weekMenu.Saturday)
    let sms = generateSms monTueSubstitutes wedThruSubstitutes friSatSubstitutes
    0 // return an integer exit code
