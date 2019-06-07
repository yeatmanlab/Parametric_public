
PsychJavaTrouble

clear all

clc

fprintf('Welcome!\n\n\n');

%subjectName = input('What is your name? ','s');
%fprintf('\n');
subjectIni = input('What is your subject ID? ','s');
fprintf('\n');
%ver = input('Experiment version (1 or 2)?  ');

%flag = input('Score board (0 = no, 1 = yes)? ');

fprintf('\n');
fprintf('Thank you!\n');
fprintf('\n');
fprintf('\n');

doPractice = 1;
trial = 1;

while doPractice
    if trial == 1
        aaa = input('Do you want to practice (y/n) ? ','s');
    else
        aaa = input('Do you want to practice more (y/n) ? ','s');
    end
    if strcmp(aaa, 'y')
        RunPractice;
        clc;
    else
        doPractice = 0;
        clc;
    end
    trial = trial + 1;
end

doRunMain = 1;
trial = 1;

while doRunMain && trial < 4
    if trial == 1
        aaa = input('Do you want to run the Space Race (y/n) ? ','s');
    else
        aaa = input('Do you want to run the Space Race again (y/n) ? ','s');
    end
    if strcmp(aaa, 'y')
        RunMotionDisc(subjectIni, trial);
        clc
        str = ['Block ' num2str(trial) ' of 3 complete!'];
        disp(str)

    else
        doRunMain = 0;
    end
    trial = trial + 1;
end