module Problem19

type DayOfWeek = Sunday | Monday | Tuesday | Wednesday | Thursday | Friday | Saturday
type DayOfMonth = DayOfMonth of int
type Month = January | February | March | April | May | June | July | August | September | October | November | December
type Year = Year of int
type Date =
    { dayOfMonth : DayOfMonth
    ; month : Month
    ; year : Year
    }
type LeapYear = IsLeapYear | NotLeapYear
type Carry = HasCarry | NoCarry

let isLeapYear y = if y % 4 = 0 && (y % 100 <> 0 || y % 400 = 0) then IsLeapYear else NotLeapYear

let getNextDayOfWeek =
    function
    | Sunday -> Monday
    | Monday -> Tuesday
    | Tuesday -> Wednesday
    | Wednesday -> Thursday
    | Thursday -> Friday
    | Friday -> Saturday
    | Saturday -> Sunday

let getNextMonth =
    function
    | January -> February
    | February -> March
    | March -> April
    | April -> May
    | May -> June
    | June -> July
    | July -> August
    | August -> September
    | September -> October
    | October -> November
    | November -> December
    | December -> January

let getDaysInMonth leap =
    function
    | September | April | June | November -> 30
    | February ->
        match leap with
        | IsLeapYear -> 29
        | NotLeapYear -> 28
    | _ -> 31

let getNextDate date =
    match date with
    | { dayOfMonth = DayOfMonth dayNumber; month = month; year = Year yearNumber } ->
        let leap = isLeapYear yearNumber
        let lastDay = getDaysInMonth leap month
        if dayNumber <> lastDay
        then { date with dayOfMonth = DayOfMonth (dayNumber + 1) }
        else
            let nextMonth = getNextMonth month
            if nextMonth <> January
            then { date with dayOfMonth = DayOfMonth 1; month = nextMonth }
            else { dayOfMonth = DayOfMonth 1; month = nextMonth; year = Year (yearNumber + 1) }

let nextPair (dayOfWeek, date) = (getNextDayOfWeek dayOfWeek, getNextDate date)

let startDate = { dayOfMonth = DayOfMonth 1; month = January; year = Year 1900 }

let result =
    Seq.initInfinite id
    |> Seq.scan (fun s _ -> nextPair s) (Monday, startDate)
    |> Seq.skipWhile (fun (_, { year = Year yearNumber }) -> yearNumber <= 1900)
    |> Seq.takeWhile (fun (_, { year = Year yearNumber }) -> yearNumber <= 2000)
    |> Seq.filter (fun (dayOfWeek, { dayOfMonth = DayOfMonth dayNumber }) -> dayOfWeek = Sunday && dayNumber = 1)
    |> Seq.length
