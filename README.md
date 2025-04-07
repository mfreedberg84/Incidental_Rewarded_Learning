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
Practice (1 block)
Training (8 blocks)
Transfer (1 block)
Questionnaire (1 block)

_Exp 1_ 
Participants made simultaneous responses to pairs of stimuli (faces) shown on a computer screen with one finger of from each hand (chords). On each trial, the left-hand response corresponded to the identity of the left stimulus and the right-hand response corresponded to the right stimulus. During training, half of the chords yielded a reward if performed correctly within the three second trial limit, while the remaining chords only yielded positive feedback if performed correctly within the time limit. Incorrect or late responses yielded negative feedback. The sole transfer block was identical to the training blocks except that 1) chords withheld from training were introduced alongside the already practiced training chords, and 2) feedback was not given. Following the transfer block, participants were presented with an explicit memory questionnaire, which included two parts. For the first part, participants were asked to judge their confidence identifying which pairs were likely to yield rewards on a scale between 0 (no confidence) and 9 (full confidence). For the second part, participants were asked to label each stimulus pair as either rewarded or unrewarded and the number of correct responses (out of 8) was recorded.

_Exp 2_ 
Participants responded to a pair of stimuli (faces) shown on a computer screen with either the right index or middle finger. During the eight training blocks, half of the  pairs yielded a reward (money) if the participant responded with the correct answer within the 5 second trial limit. The remaining pairs only yielded positive feedback ("correct") if performed correctly within the time limit. Incorrect or late repsonses yielded negative feedback ("incorrect"). The sole transfer block was identical to the training blocks except that feedback and rewards were withheld. Following the transfer block, participants were presented with an explicit memory questionnaire that was identical to Experiment 1.

The data (Incidental_Rewarded_Learning_ExpX.csv) and code (Exp_1_X.R and Exp_2_X.R) provided are intended to be used in a Jupyter notebook on Google Colab or the R coding environment.

**Exp1**
1. Training - Non-linear modeling of reaction time (RT) and accuracy (proportion correct) data across blocks (before rewards: Exp1_TrainRT and Exp1_TrainAcc).
2. Training - Non-linear modeling of reaction time (RT) and accuracy (proportion correct) data across blocks (after rewards: Exp1_TrainAfterRT and Exp1_TrainAfterAcc).
3. Transfer - One-sample, two-tailed t-tests contrasting RT and accuracy between Rewarded, Unrewarded, and Withheld Chords (Incidental_Rewarded_Learning_Exp1.ipynb).
4. Questionnaire - Pearson's correlations between subjective and force-choiced measures of explicit awareness and learning (Incidental_Rewarded_Learning_Exp1.ipynb).

**Exp2**
1. Training - Non-linear modeling of reaction time (RT) and accuracy (proportion correct) data across blocks (before rewards: Exp2_TrainRT and Exp2_TrainAcc).
2. Training - Non-linear modeling of reaction time (RT) and accuracy (proportion correct) data across blocks (after rewards: Exp2_TrainAfterRT and Exp2_TrainAfterAcc).
3. Transfer - One-sample, two-tailed t-tests contrasting RT and accuracy between Rewarded and Unrewarded Chords (Incidental_Rewarded_Learning_Exp2.ipynb).
4. Questionnaire - Pearson's correlations between subjective and force-choiced measures of explicit awareness and learning (Incidental_Rewarded_Learning_Exp2.ipynb).
