From the problem description, here's the recurrence for the number of ways to go right a single time:
> r 0 = 1
> r 1 = 1
> r 2 = 2
And since jumping by 1,2,3 is like repeating the problem on a smaller N:
> r n = (r $ n-1) + (r $ n-2) + (r $ n-3)

M is the number of times to repeat the roundtrip:
F(M,N) = F(1,N)^M
And doing a roundtrip is like doing a rightward trip twice:
F(1,N) = R(N)^2

So:
> f m n = (r n)^(2 ⋅ m)

Rewrite it like this:
r(n+3) = r(n) + r(n+1) + r(n+2)

Then:
(R(x) - (2x²+x+1))/x^3
R(x) = x ⋅ R(x) + x² ⋅ R(x) + x³ ⋅ R(x)
R(x) = (x + x² + x³) ⋅ R(x)