-- transpose matrix defined as list of lists
-- Validate if for one number x all function will be applicable, return true
-- ValidateNumber 1 [(\x -> x < 4), (\x -> x*x == x)] == True
-- We have [(City, Temperaure)] defined by [(Integer, [Char])]. Find the name of the city most close up to the average one into the list
-- Implement quick sort

{--
Напишете ф-я lSystem axiom rules, която да връща безкраен списък от стрингове със следните свойства:
axiom е стринг.
rules е списък от наредени двойки (Char, [Char])
Всяки следващ стринг трябва да е получен чрез заместване на символите от предния стринг със стрингове от правилата.
Първият стринг в безкрайният списък от състояния е аксиомата.
Пример:

axiom = 'A'
rules = [('A', "AB"), ('B', "A")]
last $ take 7 $ lSystem axiom rules
> ABAABABAABAABABAABABAABAABABAABAAB
The task is from fmi-fp-2020-21
--}