import Ratio
import Char

double :: Int -> Int
double x = x + x
quad x = double (double x)

fact n = product[1..n]


average ns = True

average :: Bool -> Bool

mul a b c = a * b * c
fm1 = mul 3
fm2 = fm1 5
fm3 = fm2 7

data Day = M | Tu | W | Th | F | Sa | Su
 deriving (Eq, Show,Enum)
 
isWeekend :: Day -> Bool
isWeekend x = (x == Sa || x == Su)

nextDay :: Day -> Day
nextDay d = head(tail[d..Su] ++ [M])
nextDay2 d = last(init(reverse([d..Su] ++[M])))

data Direction = Kuzey | Guney
 deriving (Eq, Show,Enum)
 
degree :: Direction -> Int
degree d = [90,180] !! (length[Kuzey .. d] - 1)

abs2::Int->Int
abs2 n = if n >= 0 then n else -n

signum::Int->Int
signum n = if n < 0 then -1 else
           if n == 0 then 0 else 1
		   
signum2 n | n < 0 = -1 | n == 0 = 0 | otherwise = 1

--PATTERN MATCHING [part 1]
not2::Bool->Bool
not2 False = True --formal degişken kullandık
not2 True = False

(&&)::Bool->Bool->Bool
True && True = True
_ && False = False
False && _ = False
  
--LIST PATTERNS [part 2]
fpt [] = 1         --liste bos
fpt [x] = 2        --1 eleman var
fpt (x:xs) = 3     --en az 1 eleman var
fpt (x:y:xs) = 4   --en az 2 eleman var
fpt [x,y] = 5      --2 eleman var

--Fonskiyon çağrıldığında önce hangisi eşlenirse o döner.
--fpt [5,6,4] için 3,4 eşlenir, ancak 3 önce tanımlı olduğundan geriye 3 döner.

head2::[a]->a
head2 (x:xs) = x

tail2 (x:xs) = xs
--tail2 (_:xs) = xs  (fark yok)

--INTEGER PATTERNS [part 3]
azalt (n+3) = n --Verilen sayıyı 3 azaltır. n = 2 ve sonrası için hata verir. Negatif Int tipinde değil 
arttir (n) = n+1

--LAMBDA EXPRESSIONS [part 4]
--Main içerisinde çalıştır
-- (\x -> x * 2) 5           Son eleman bize x'i gösteriyor, çıktı 10 olur
-- (\ys -> (\x -> x ^ (sum ys)) 4) [0..5]    --x = 4, ys ise dizinin değerini alır.

add2 x = \y -> x + y

--n tane tek sayı üretir
odds n = map f[0..n-1] 
         where f x = x * 2 + 1

odds2 n = map(\x -> x * 2 + 1) [0..n-1]
--map(f) map fonksiyonunun içine fonksiyon yazılır ve uygulanacak girdileri verilir.


--SECTIONS  [part 5]
-- (/) 9 3   denktir (/3) 9 denktir (9/) 3


--EGZERSIZLER

safetail xs | null xs = []
            | otherwise = tail xs

safetail2 [] = []
safetail2 (x:xs) = xs --pattern kullandık


--SET COMPREHENSIONS  [part 6]
--geriye bir liste döndürürüz
--matematikte olan {x | x elemanıdır {1,2,3}} gibi bir ifadenin karşılığıdır

s = [x*y | x<-[1,2], y<-[5..7]] --önce x'deki for'dan başlar
--for(x = 2)
--for(y = 3)
--cout x*y    

concat2::[[a]] -> [a]
concat2 xss = [x | xs<-xss , x <- xs]

--GUARDS [part 7]
ciftler xs = [x | x<-xs, even x]
bolenler n = [x | x<-[1..n], mod n x == 0]
isprime n = bolenler n == [1,n]

--optimize bir sekilde bolen bulma kodu
--

--ZIP FUNCTION
--zip::[a] -> [b] -> [(a,b)]  --skalar şekilde elemanları eşler

--bir dizinin elemanları sıralımı diye kontrol edelim
pairs2::[a] -> [(a,a)]
pairs2 xs = [(x,y) | (x,y)<-zip xs (tail xs)]

--siralimi::[a] -> Bool
siralimi xs = null [(x,y) | (x,y)<-pairs2 xs, x > y ]
siralimi2 xs = and [x <= y | (x,y) <- pairs2 xs]


--STRING COMPREHENSIONS [part 8]
todigit n = [[c] | c <- show(n)]
todigit2 n = [ord c - ord '0' | c<-show(n)] --ne yapar bilmiyorum..
-- ord '0' bize sıfırın ascii kodunu döndürür

--EXERCISES
--EGZERSIZ1  x^2 + y^2 = z^2   üçlü tuple'ları bulan kodu yazılır
egzersiz1 n = [ (x,y,z) |x <- [1..n], y <- [x..n], z <- [y..n], x^2 + y^2  == z^2]
--burada en buyuk z olur, y x'den buyuk 

--EGZERSIZ2  mukemmel sayilari bulan fonksiyonu yaz. Kendisi haric tum bolenleri
--toplamı kendisine esit ise mukemmel sayidir.

ismukemmel n = sum (bolenler (n)) - n == n
egzersiz2 n = [x | x <- [1..n], ismukemmel x]

--EGZERSIZ3  iki listenin skalar carpimini hesapla
egzersiz3 xs ys = sum [x*y | (x,y) <- zip xs ys]


--REKURSIF FONKSIYONLAR [part 9]
fact2 0 = 1;
fact2 n = n * fact2 (n-1)

fact3 n = 
      let --fonksiyon tanimi
	   afact3 (m,0) = m 
	   afact3 (m,n) = afact3(m*n,n-1)
	  in
	   afact3 (1,n) --baslangic durumu
	   

product2 [] = 1
product2 (x:xs) = x * product2 xs

product3 xs =
  let
    aproduct3 (m, [])     = m
    aproduct3 (m, x : xs) = aproduct3 (m * x, xs)
  in
    aproduct3 (1, xs)
	   
	  
length2 xs = 
    let 
	alength2 (m,[]) = m 
	alength2 (m,x:xs) = alength2(m+1,xs)
	in
	alength2 (0,xs)

reserve2 xs = 
    let 
	areserve2 ([],xs) = xs
	areserve2 (n:ns, xs) = areserve2(ns, n:xs)
	in
	areserve2 (xs,[])
	
--quick sort,   önemli
qsort []     = []
qsort (x:xs) =
  qsort smaller ++ [x] ++ qsort larger
  where
    smaller = [a | a <- xs, a <= x]
    larger  = [b | b <- xs, b > x]


--[CHAPTER 8] Polymorphizm Çokbiçimlilik
--fonksiyonu en az iki farklı veri tipi üzerinde kullanabilmektir.

--overloading (iki farklı fonksiyon tanımlama)
--parametre dönüşümü (gizli tür dönüşümü)
--parametrik çokbiçimlilik (type variable kullanılması)
--alt tür çokbiçimlilik  (classlardaki türetilme olayları gibi)

--[CHAPTER 9]
--YÜKSEK MERTEBEDEN FONKSİYONLAR
--iç içe fonksiyonlardan oluşur. 
--         f       x    geri döndürülen
-- twice (a->a) -> a -> a
-- bir f girdisi ve değişken girdisi alır geriye değer döndürür
twice f x = f (f x)
--twice not False       Çıktı: False

--  map::(a->b)->[a]->[b]
--  map (+1) [1,3,5]    Çıktı: [2,4,6]


--FILTER FUNCTION
--  fiter::(a->Bool)->[a]->[a]
--  filter (>3) [1,5,8,9]       Çıktı: [5,8,9]

filter2 p [] = []
filter2 p(x:xs) 
 | p x = x : filter2 p xs 
 | otherwise = filter2 p xs
-- p burada koşulumuz olur, eğer p (>3) olursa bu koşula göre kontrol yapar


-- sayının 2 üzeri maksimum kaca kadar ifade edilebilecegini bulur
toBin n = (filter ((<=n).(2^))) [0..n]
toBin2 n = (takeWhile ((<=n).(2^))) [0..n]  --filter'a göre çok daha hızlı
--koşulu sağlamayan 1 tane elemanla bile karşılaşırsa listenin geri
--kalanına bakmaz. bu sayede optimize çalışır.
--binarye donusturen fonksiyonu tamamla.. 50.00


-- THE FOLDR FUNCTION
{-
foldl (\x y -> 2*x+y) 0 [1..5]
0 1 = 1
1 2 = 4
4 3 = 11
...
eşitlikten çıkanımız soldakine aktarılır yani x'e
0 değeri en sola yerleştirilir.

foldr (\x y -> 2*x+y) 0 [1..5]

bu sefer 0 değeri en sağdakine yani y'ye aktarılır.
sağdan geldiğimiz için x yerine 5 yazılır ilk başta

5 0 = 10
4 10 = 18
...
-}
length3 xs = foldl (\l x-> l+1) 0 xs

-- ALL FUNCTION
--listenin elemanları verilen koşulu sağlarsa true döndürür.
-- all even [2,4,6,8]     Çıktı: True
-- ANY FUNCTION 
-- listenin en az 1 elamanı verilen koşulu sağlarsa

-- dropWhile
-- koşul sağlandığı sürece listenin başından eleman atar 
