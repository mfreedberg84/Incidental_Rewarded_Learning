# Incidental_Rewarded_Learning

Reward has been shown to change behavior as a result of incentive learning (by motivating the individual
to increase their effort) and instrumental learning (by increasing the frequency of a particular behavior).
However, Palminteri et al. (2011) demonstrated that reward can also improve the incidental learning of
a motor skill even when participants are unaware of the relationship between the reward and the motor
act. Nonetheless, it remains unknown whether these effects of reward are the indirect results of
manipulations of top– down factors. To identify the locus of the benefit associated with rewarded
incidental learning, we used a chord-learning task (Seibel, 1963) in which the correct performance of
some chords was consistently rewarded with points necessary to complete the block whereas the correct
performance of other chords was not rewarded. Following training, participants performed a transfer
phase without reward and then answered a questionnaire to assess explicit awareness about the rewards.
Experiment 1 revealed that rewarded chords were performed more quickly than unrewarded chords, and
there was little awareness about the relationship between chords and reward. Experiment 2 obtained
similar findings with simplified responses to show that the advantage for rewarded stimulus combinations
reflected more efficient binding of stimulus–response (S-R) associations, rather than a response bias for
rewarded associations or improved motor learning. These results indicate that rewards can be used to
significantly improve the learning of S-R associations without directly manipulating top– down factors.

**Experimental Design**
11 Blocks:
Block 1 - Practice
Blocks 2-9 - Training
Block 10 - Transfer
Block 12 - Questionnaire

_Exp 2_ 
Participants responded with either the right index or middle on a serial response box to two face stimuli shown on a computer screen. During training half of the stimulus pairs yielded a reward if performed correctly within the 3 second trial limit, while the remaining chords only yielded positive feedback if performed correctly  within the time limit. Incorrect or late repsonses yielded negative feedback. The sole transfer block was identical to the training blocks except that feedback was not given. The questionnaire was identical to Experiment 1

The data (Incidental_Rewarded_Learning_Exp2.csv) and code (Exp_2_X.R) provided are intended to be used in a Jupyter notebook or the R coding environment.

**Exp2**
1. Training - Non-linear modeling of reaction time (RT) and accuracy (proportion correct) data across blocks (before rewards: Exp2_TrainRT and Exp2_TrainAcc).
2. Training - Non-linear modeling of reaction time (RT) and accuracy (proportion correct) data across blocks (after rewards: Exp2_TrainAfterRT and Exp2_TrainAfterAcc).
3. Transfer - One-sample, two-tailed t-tests contrasting RT and accuracy between Rewarded and Unrewarded Chords (Incidental_Rewarded_Learning_Exp2.ipynb).
4. Questionnaire - Pearson's correlations between subjective and force-choiced measures of explicit awareness and learning (Incidental_Rewarded_Learning_Exp2.ipynb).
