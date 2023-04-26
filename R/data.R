# nolint start: line_length_linter.

#' PODCI Example Data
#'
#' @description
#' Example Pediatric Outcome Data Collection Instrument (PODCI) Data
#'
#' @format
#' A 3 x 86 [dplyr::tibble]
#'
#' 1. During last week, easy/hard to: Lift heavy books?
#'     * 1, Easy | 2, A little hard | 3, Very hard | 4, Can't do at all | 5, Too young for this activity
#' 1. During last week, easy/hard to: Pour a half gallon of milk?
#'     * 1, Easy | 2, A little hard | 3, Very hard | 4, Can't do at all | 5, Too young for this activity
#' 1. During last week, easy/hard to: Open a jar that has been opened before?
#'     * 1, Easy | 2, A little hard | 3, Very hard | 4, Can't do at all | 5, Too young for this activity
#' 1. During last week, easy/hard to: Use a fork and spoon?
#'     * 1, Easy | 2, A little hard | 3, Very hard | 4, Can't do at all | 5, Too young for this activity
#' 1. During last week, easy/hard to: Comb his/her hair?
#'     * 1, Easy | 2, A little hard | 3, Very hard | 4, Can't do at all | 5, Too young for this activity
#' 1. During last week, easy/hard to: Button buttons?
#'     * 1, Easy | 2, A little hard | 3, Very hard | 4, Can't do at all | 5, Too young for this activity
#' 1. During last week, easy/hard to: Put on his/her coat?
#'     * 1, Easy | 2, A little hard | 3, Very hard | 4, Can't do at all | 5, Too young for this activity
#' 1. During last week, easy/hard to: Write with a pencil?
#'     * 1, Easy | 2, A little hard | 3, Very hard | 4, Can't do at all | 5, Too young for this activity
#' 1. Over the last 12 months, how often did child miss school because of health?
#'     * 1, Rarely | 2, Once a month | 3, Two or three times a month | 4, Once a week | 5, More than once a week | 6, Does not attend school
#' 1. During last week, how happy with: looks?
#'     * 1, Very happy | 2, Somewhat happy | 3, Not sure | 4, Somewhat unhappy | 5, Very unhappy | 6, Child is too young
#' 1. During last week, how happy with: body?
#'     * 1, Very happy | 2, Somewhat happy | 3, Not sure | 4, Somewhat unhappy | 5, Very unhappy | 6, Child is too young
#' 1. During last week, how happy with: clothes or shoes can wear?
#'     * 1, Very happy | 2, Somewhat happy | 3, Not sure | 4, Somewhat unhappy | 5, Very unhappy | 6, Child is too young
#' 1. During last week, how happy with: ability to do the same things friends do?
#'     * 1, Very happy | 2, Somewhat happy | 3, Not sure | 4, Somewhat unhappy | 5, Very unhappy | 6, Child is too young
#' 1. During last week, how happy with: health in general?
#'     * 1, Very happy | 2, Somewhat happy | 3, Not sure | 4, Somewhat unhappy | 5, Very unhappy | 6, Child is too young
#' 1. During last week, how often: feel sick and tired?
#'     * 1, Most of the time | 2, Some of the time | 3, A little of the time | 4, None of the time
#' 1. During last week, how often: full of pep and energy?
#'     * 1, Most of the time | 2, Some of the time | 3, A little of the time | 4, None of the time
#' 1. During last week, how often: pain or discomfort interfere with activities?
#'     * 1, Most of the time | 2, Some of the time | 3, A little of the time | 4, None of the time
#' 1. During last week, easy/hard to: Run short distances?
#'     * 1, Easy | 2, A little hard | 3, Very hard | 4, Can't do at all | 5, Too young for this activity
#' 1. During last week, easy/hard to: Bicycle or tricycle?
#'     * 1, Easy | 2, A little hard | 3, Very hard | 4, Can't do at all | 5, Too young for this activity
#' 1. During last week, easy/hard to: Climb three flights of stairs?
#'     * 1, Easy | 2, A little hard | 3, Very hard | 4, Can't do at all | 5, Too young for this activity
#' 1. During last week, easy/hard to: Climb one flight of stairs?
#'     * 1, Easy | 2, A little hard | 3, Very hard | 4, Can't do at all | 5, Too young for this activity
#' 1. During last week, easy/hard to: Walk more than a mile?
#'     * 1, Easy | 2, A little hard | 3, Very hard | 4, Can't do at all | 5, Too young for this activity
#' 1. During last week, easy/hard to: Walk three blocks?
#'     * 1, Easy | 2, A little hard | 3, Very hard | 4, Can't do at all | 5, Too young for this activity
#' 1. During last week, easy/hard to: Walk one block?
#'     * 1, Easy | 2, A little hard | 3, Very hard | 4, Can't do at all | 5, Too young for this activity
#' 1. During last week, easy/hard to: Get on and off a bus?
#'     * 1, Easy | 2, A little hard | 3, Very hard | 4, Can't do at all | 5, Too young for this activity
#' 1.  How often need help from another person for walking and climbing?
#'     * 1, Never | 2, Sometimes | 3, About half the time | 4, Often | 5, All the time
#' 1.  How often use assistive devices for walking and climbing?
#'     * 1, Never | 2, Sometimes | 3, About half the time | 4, Often | 5, All the time
#' 1. During last week, easy/hard to:  Stand while washing hands and face at a sink?
#'     * 1, Easy | 2, A little hard | 3, Very hard | 4, Can't do at all | 5, Too you young for this activity
#' 1. During last week, easy/hard to:  Sit in a regular chair without holding on?
#'     * 1, Easy | 2, A little hard | 3, Very hard | 4, Can't do at all | 5, Too you young for this activity
#' 1. During last week, easy/hard to:  Get on and off a toilet or chair?
#'     * 1, Easy | 2, A little hard | 3, Very hard | 4, Can't do at all | 5, Too you young for this activity
#' 1. During last week, easy/hard to:  Get in and out of bed?
#'     * 1, Easy | 2, A little hard | 3, Very hard | 4, Can't do at all | 5, Too you young for this activity
#' 1. During last week, easy/hard to:  Turn door knobs?
#'     * 1, Easy | 2, A little hard | 3, Very hard | 4, Can't do at all | 5, Too you young for this activity
#' 1. During last week, easy/hard to:  Bend over from a standing position and pick up something off the floor?
#'     * 1, Easy | 2, A little hard | 3, Very hard | 4, Can't do at all | 5, Too you young for this activity
#' 1.  How often need help from another person for sitting and standing?
#'     * 1, Never | 2, Sometimes | 3, About half the time | 4, Often | 5, All the time
#' 1.  How often use assistive devices for sitting and standing?
#'     * 1, Never | 2, Sometimes | 3, About half the time | 4, Often | 5, All the time
#' 1. Participate in recreational outdoor activities with other children the same age?
#'     * 1, Yes easily | 2, Yes but a little hard | 3, Yes but very hard | 4, No
#' 1. Was activity limited by: Pain?
#'     * 1, Yes
#' 1. Was activity limited by: General Health?
#'     * 1, Yes
#' 1. Was activity limited by: Doctor or parent instructions?
#'     * 1, Yes
#' 1. Was activity limited by: Fear the other kids won’t like him/her?
#'     * 1, Yes
#' 1. Was activity limited by: Dislike of recreational outdoor activities?
#'     * 1, Yes
#' 1. Was activity limited by: Too young?
#'     * 1, Yes
#' 1. Was activity limited by: Activity not in season?
#'     * 1, Yes
#' 1. Participate in pickup games or sports with other children the same age?
#'     * 1, Yes easily | 2, Yes but a little hard | 3, Yes but very hard | 4, No
#' 1. Was activity limited by: Pain?
#'     * 1, Yes
#' 1. Was activity limited by:  General Health?
#'     * 1, Yes
#' 1. Was activity limited by:  Doctor or parent instructions?
#'     * 1, Yes
#' 1. Was activity limited by:  Fear the other kids won’t like him/her?
#'     * 1, Yes
#' 1. Was activity limited by:  Dislike of pickup games or sports?
#'     * 1, Yes
#' 1. Was activity limited by:  Too young?
#'     * 1, Yes
#' 1. Was activity limited by:  Activity not in season?
#'     * 1, Yes
#' 1. Participate in competitive level sports with other children the same age?
#'     * 1, Yes, easily | 2, Yes, but a little hard | 3, Yes, but very hard | 4, No
#' 1. Was activity limited by:  Pain?
#'     * 1, Yes
#' 1. Was activity limited by:  General Health?
#'     * 1, Yes
#' 1. Was activity limited by:  Doctor or parent instructions?
#'     * 1, Yes
#' 1. Was activity limited by:  Fear the other kids won’t like him/her?
#'     * 1, Yes
#' 1. Was activity limited by:  Dislike of competitive level sports?
#'     * 1, Yes
#' 1. Was activity limited by:  Too young?
#'     * 1, Yes
#' 1. Was activity limited by:  Activity not in season?
#'     * 1, Yes
#' 1.  How often in last week did child get together and do things with friends?
#'     * 1, Often | 2, Sometimes | 3, Never or rarely
#' 1. Was activity limited by:  Pain?
#'     * 1, Yes
#' 1. Was activity limited by:  General Health?
#'     * 1, Yes
#' 1. Was activity limited by:  Doctor or parent instructions?
#'     * 1, Yes
#' 1. Was activity limited by:  Fear the other kids won’t like him/her?
#'     * 1, Yes
#' 1. Was activity limited by:  Friends not around?
#'     * 1, Yes
#' 1.  How often in last week did child participate in gym/recess?
#'     * 1, Often | 2, Sometimes | 3, Never or rarely | 4, No gym or recess
#' 1. Was activity limited by:  Pain?
#'     * 1, Yes
#' 1. Was activity limited by:  General Health?
#'     * 1, Yes
#' 1. Was activity limited by:  Doctor or parent instructions?
#'     * 1, Yes
#' 1. Was activity limited by:  Fear the other kids won’t like him/her?
#'     * 1, Yes
#' 1. Was activity limited by:  Dislike of gym/recess?
#'     * 1, Yes
#' 1. Was activity limited by:  School not in session?
#'     * 1, Yes
#' 1. Was activity limited by:  Does not attend school?
#'     * 1, Yes
#' 1.  Is it easy or hard to make friends with children own age?
#'     * 1, Usually easy | 2, Sometimes easy | 3, Sometimes hard | 4, Usually hard
#' 1.  How much pain during the last week?
#'     * 1, None | 2, Very mild | 3, Mild | 4, Moderate | 5, Severe | 6, Very severe
#' 1.  During last week, how much did pain interfere with normal activities
#'     * 1, Not at all | 2, A little bit | 3, Moderately | 4, Quite a bit | 5, Extremely
#' 1. Expectation of Treatment: To have pain relief.
#'     * 1, Definitely yes | 2, Probably yes | 3, Not sure | 4, Probably not | 5, Definitely not
#' 1. Expectation of Treatment: To look better.
#'     * 1, Definitely yes | 2, Probably yes | 3, Not sure | 4, Probably not | 5, Definitely not
#' 1. Expectation of Treatment: To feel better about himself/herself.
#'     * 1, Definitely yes | 2, Probably yes | 3, Not sure | 4, Probably not | 5, Definitely not
#' 1. Expectation of Treatment: To sleep more comfortably.
#'     * 1, Definitely yes | 2, Probably yes | 3, Not sure | 4, Probably not | 5, Definitely not
#' 1. Expectation of Treatment: To be able to do activities at home.
#'     * 1, Definitely yes | 2, Probably yes | 3, Not sure | 4, Probably not | 5, Definitely not
#' 1. Expectation of Treatment: To be able to do more at school.
#'     * 1, Definitely yes | 2, Probably yes | 3, Not sure | 4, Probably not | 5, Definitely not
#' 1. Expectation of Treatment: To be able to do more play or recreational activities.
#'     * 1, Definitely yes | 2, Probably yes | 3, Not sure | 4, Probably not | 5, Definitely not
#' 1. Expectation of Treatment: To be able to do more sports.
#'     * 1, Definitely yes | 2, Probably yes | 3, Not sure | 4, Probably not | 5, Definitely not
#' 1. Expectation of Treatment: To be free from pain or disability as an adult.
#'     * 1, Definitely yes | 2, Probably yes | 3, Not sure | 4, Probably not | 5, Definitely not
#' 1. If child had to spend the rest of his/her life with his/her bone and muscle condition as it is right now, how would you feel about it?
#'     * 1, Very satisfied | 2, Somewhat satisfied | 3, Neutral | 4, Somewhat dissatisfied | 5, Very dissatisfied
"podci"

# nolint end
