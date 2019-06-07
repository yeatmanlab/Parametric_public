function RunMotionDisc(SubjectCode, blocknum)
KbQueueCreate;
KbQueueStart;
[id,name] = GetKeyboardIndices;
%deviceString = 'Logitech Logitech Illuminated Keyboard';
deviceString = 'Apple, Inc Apple Keyboard';
% KbName('UnifyKeynames')

% Get the index for the correct input
for i=1:length(name)%for each possible device
    if strcmp(name{i},deviceString)%compare the name to the name you want     
        device=id(i);%grab the correct id, and exit loop
        break;
    end
end

% Which keys are we enabling?
keys = [115,117,116,10 ]; % down arrow, right arrow, and space. 
keylist = zeros(1,256); % create a list of 256 zeros
keylist(keys) = 1; % set the keys we want to monitor to 1

% Make the KB Queue
KbQueueCreate(device, keylist); 
KbQueueStart() % Start listening


mainDir = pwd;
%%
kb = InitKeyboard;

mt_prepSounds;

% Preferences for display 
display.bkColor = [128 128 128];
display.fixColor = [0 0 0];
display.dist = 56;
display.width = 53;
display.widthInVisualAngle = 2*atan(display.width/2/display.dist) * 180/pi;
display = OpenWindow(display);



display.fixShape = 'bullseye';
display.fixSize = 1*display.ppd; 
display.fixation = [display.cx display.cy];

if display.frameRate == 120
    dots.dotLife = 24; %frames
else
    dots.dotLife = 12;
end

% Make the mouse invisible
HideCursor(display.wPtr);

% Parameters for the dots
dots.nDots = 150;
dots.speed = 8;  %speeds for each field deg/sec
dots.color = [0 0 0]; %colors for each field [r,g,b]
dots.size = 5; %pixels
dots.wSize = 7;
dots.fixBkSize = 1;
totalDots = sum(dots.nDots); % Total number of dots
dots.xy = zeros(2,totalDots);
dots.dxdy = zeros(2,totalDots);
dots.dotAge = zeros(1,totalDots);
dots.dotSize = zeros(1,totalDots); 
dots.dotColor = zeros(3,totalDots);
dots.rmax = dots.wSize;	% maximum radius of annulus (pixels from center)
dots.rmin = dots.fixBkSize; % minimum
dots.dur = 10; % 600 ms
dots.nFrames = round(dots.dur / display.ifi);
dots.center = [display.cx display.cy];


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Preferences for dot coherence presentations
coherence_vector = [0.06 0.12 0.24 0.48]; % The coherence values in the study
config.nTrialsPer = 10; % Number of presentations per coherence condition

end_pts = [1];
config.nEndPer = 10;



config.nTotalTrials = config.nTrialsPer * length(coherence_vector) + config.nEndPer * length(end_pts); % Number of trials
config.coherence = sort([repmat(coherence_vector, 1, config.nTrialsPer), repmat(end_pts, 1, config.nEndPer)]);

% Make sure each stimulus is presented an equal number of times in each
% direction


config.dir = repmat([0 1], 1, config.nTotalTrials/2) .* 180;
% Shuffle them with the same indices so that every coherence level has
% equal presentations in each direction
p = randperm(config.nTotalTrials);
config.coherence = config.coherence(p);
config.dir = config.dir(p);


% Set up the results structure
result.response = NaN * ones(1,config.nTotalTrials);
result.keyResponse = NaN * ones(1,config.nTotalTrials);
result.RT = NaN * ones(1, config.nTotalTrials);

 
% Initialize trial number for count
trial = 1;

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Show the instructions
instrIm = imread('Instructions.jpg');


scaleFactor = display.wRect(3)/size(instrIm,2);
srcRect = [0 0 size(instrIm,2)-1 size(instrIm,1)-1];
destRect = CenterRectOnPoint(ScaleRect(srcRect,scaleFactor,scaleFactor),display.cx,display.cy);
instTexture = Screen('MakeTexture', display.wPtr, instrIm);
Screen('DrawTexture', display.wPtr, instTexture,srcRect,destRect);
Screen('Flip', display.wPtr);    
    
    
    


% Wait for the space bar to be pushed to continue
wait4Space = 0;
while ~wait4Space
    [keyIsDown, secs, keyCode, deltaSecs] = KbCheck(-1);
    if keyIsDown && keyCode(kb.spaceKey)
        wait4Space = 1;
    end
end

score = 0;
%% Run Trials
while trial <= config.nTotalTrials % While the number of trials is less than the total
     KbQueueFlush();
    % Determine the dot coherence for the upcoming trial
    dots.coherence = config.coherence(trial);
    dots = CreateDots(dots,config.dir(trial),display);
    
    MakeFixation(display);
    Screen('Flip', display.wPtr);
    WaitSecs(.5);
    
    j = 1; t = GetSecs; isResponded = 0;

    % While the dots are on display and the keypress has not occurred
    while j < dots.dur*display.frameRate && ~isResponded

        % Draw the dots on the screen
        try m = dots.xy(:,:,j) * display.ppd;
        catch
            disp(j)
            disp(size(dots.xy))
            disp(display.ppd)
            break
        end

        Screen('DrawDots',  display.wPtr, m, dots.size, dots.dotColor, dots.center, 2);
        MakeFixation(display);
        Screen('Flip', display.wPtr);
        j = j + 1; % Advance frame number
        

        [pressed, firstPress] = KbQueueCheck();
        if pressed % if key is down
            if strcmp(KbName(firstPress), 'DownArrow') 
                result.keyResponse(trial) = 1;
                isResponded = 1;
                result.RT(trial) = GetSecs - t;
            elseif strcmp(KbName(firstPress), 'RightArrow')
                result.keyResponse(trial) = 0;
                isResponded = 1; 
                result.RT(trial) = GetSecs - t;
            elseif strcmp(KbName(firstPress), 'ESCAPE')
                Screen('CloseAll');
                   ListenChar(0);
                ShowCursor;
                break;
            end
            
        end
        
        % Wait if it's not time yet
        while GetSecs-t < (j-1)*display.ifi; end
        
    end
    result.motionDur(trial) = GetSecs - t;
    
    MakeFixation(display);
    Screen('Flip', display.wPtr);
    
    % Wait for keypress to continue 
    while ~isResponded
        
        [pressed, firstPress] = KbQueueCheck();
        if pressed % if key is down
            if strcmp(KbName(firstPress), 'DownArrow')
                result.keyResponse(trial) = 1;
                isResponded = 1;
                result.RT(trial) = GetSecs - t;
            elseif strcmp(KbName(firstPress), 'RightArrow')
                result.keyResponse(trial) = 0;
                isResponded = 1;
                result.RT(trial) = GetSecs - t;
            elseif strcmp(KbName(firstPress), 'ESCAPE')
                Screen('CloseAll');
                ListenChar(0);
                ShowCursor;
                PsychPortAudio('Close')
                RestrictKeysForKbCheck([]); % Re-enable all keys
                break;
            end
        end
        
    end
    
    result.response(trial) = result.keyResponse(trial) == config.dir(trial)/180;
    
    % Give the particpant feedback- yay! or not quite.
    if result.response(trial)
        PsychPortAudio('Start', 0, 1, 0, 1);
        str = '+3';
        textBounds = Screen('TextBounds',display.wPtr,str);
        Screen('DrawText',display.wPtr, str, display.cx - round((textBounds(3)-textBounds(1))/2), ...
            display.cy - round((textBounds(4)-textBounds(2))/2),[0 255 0]);
        score = score + 1;
    else
        PsychPortAudio('Start', 2, 1, 0, 1);
        str = '+0';
    
        textBounds = Screen('TextBounds',display.wPtr,str);
        Screen('DrawText',display.wPtr, str, display.cx - round((textBounds(3)-textBounds(1))/2), ...
            display.cy - round((textBounds(4)-textBounds(2))/2),[255 0 0]);
    end 
    Screen('Flip',display.wPtr);   
    trial = trial + 1;
    % Pause for a second between trials
    WaitSecs(1);
end

if isempty(dir('Data'))
    mkdir('Data');
end
cd('Data');

%% Save the results
 results_file = [SubjectCode '_' datestr(now, 'yyyymmdd-HHMM') '.csv'];

% Write CSV file
output_pointer = fopen(results_file, 'w');
data_header_row = 'SubjectID,Trial,Coherence,Direction,Response,RT';
fprintf(output_pointer, '%s\n',data_header_row);
 
output_pointer = fopen(results_file, 'a');
for i = 1:length(config.dir)
fprintf(output_pointer, '%s,%d,%d,%d,%d,%d\n',...
    SubjectCode, ... %s
    i, ... %d
    config.coherence(i), .... %d,
    config.dir(i), ... %d
    result.response(i), ... %d
    result.RT(i)); %d
end
fclose(output_pointer);


% Save a .mat file
fn = sprintf('%s-%s.mat',SubjectCode,datestr(now,'yyyymmdd-HHMM'));
save(fn,'display','config','result','dots');
cd(mainDir); 


% Show the level up file
fid = ['Success_Lvl_' num2str(blocknum) '.jpg'];
lvlIm = imread(fid);


scaleFactor = display.wRect(3)/size(lvlIm,2);
srcRect = [0 0 size(lvlIm,2)-1 size(lvlIm,1)-1];
destRect = CenterRectOnPoint(ScaleRect(srcRect,scaleFactor,scaleFactor),display.cx,display.cy);
bkTexture = Screen('MakeTexture', display.wPtr, lvlIm);
Screen('DrawTexture', display.wPtr, bkTexture,srcRect,destRect);
Screen('Flip', display.wPtr);

WaitSecs(3)

 

Screen('CloseAll');
ShowCursor;
ListenChar(0);
RestrictKeysForKbCheck([]); % Re-enable all keys
PsychPortAudio('Close');
end 
