# addm_writeup
chapter 1 write up

Anna: done on 30th May

New comments from Neil May 22:

1. On p. 14, explain Figure 3 in more detail (perhaps this comes later). So talk the reader through the different manipulations and how the four models make different predictions.

2. p. 15. Yes, this is okay!

3. Figure 5 caption or there abouts. Participants chose by pressing ... . We did not add a text prompt to the screen to avoid adding additional things for people to look at.

4. p. 21, "For example, if there is just one..." It can't be bad to have more rather than fewer simulations.

p. 20. "We first describe the standard approach used, so that we can highlight how our two empirical approaches are an improvement and address issues with the standard approach."

p. 21. I think we need a paragraph beginning, "So, to summarise the novel aspects in our stochastic simulation approach, we model individual trials from individual participants, and use the eye movements specific to the very trial we are modelling." Or something. And again at the end of 6.2 we need some summary about how awesome we are.

p. 22. "To circumvent the problem arising from using such a high number of simula-
tions". It is not the high number that is the problem, it is the stochasticity. 

We need to explain that rather than deriving the probability of the data through noisy simulation, we calculate it directly, and thus remove all of the issues about the stochasticity in each step in the Nelder Mead making the error surface jump about, thus making the search much easier.

That is, we are using a closed-form solution for the probability, and using a numerical integration technique to implement the closed-form.

Refer to fig 8 in the main text on p. 26 in paragraph starting "in each trial..." (which should be "On each trial...")

Can you mess with alpha for the dots and make the yellow more yellow so people can see the density diffuse in Figure 8? Not critical... Or describe what people can see in F8 in words in the last paragraph on p. 27. "At time step one, the density has diffused from the central (0,0) state. At time step two, this density has further diffused. and so on. The density is drifting (say direction, e.g., north east, as evidence for Option X builds) and diffusing.

I think we should say, at the intro for Expt 1, that our contribution is two-fold. The first is the answer to the tranform question, which you have clearly explained, and the second is the development of the probability density evolution approach to model estimation.

"since the movie stimuli is somewhat complex" are somewhat



