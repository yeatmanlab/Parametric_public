function RunPractice


mainDir = pwd;
%% Initialize keypress
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






mt_prepSounds;
kb = InitKeyboard;
 
bkIm = imread('Instruction2.jpg');



%%
% subjectName = input('Your name? : ', 's');


display.bkColor = [128 128 128];
display.fixColor = [0 0 0];

display.dist = 56;
display.width = 53;
display.widthInVisualAngle = 2*atan(display.width/2/display.dist) * 180/pi;

display.screenNum = max(Screen('Screens'));

display = OpenWindow(display);
HideCursor(display.wPtr);
ListenChar(2);


% Define black, white and gray
black = BlackIndex(display.screenNum);
white = WhiteIndex(display.screenNum);
gray = white/2;



display.fixShape = 'bullseye';
display.fixSize = 1*display.ppd;
% display.fixation = [display.cx display.cy];

if display.frameRate == 120
    dots.dotLife = 24; %frames
else
    dots.dotLife = 12;
end

dots.nDots = 130;
dots.speed = 8;  %speeds for each field deg/sec
dots.color = [0 0 0]; %colors for each field [r,g,b]
dots.size = 5; %pixels
dots.wSize = 6.5;
dots.fixBkSize = 1;
% dots.window = dots.wSize*[-1,1,1,-1]; %aperture [l,r,t,b] from center (degrees)

% dots.nFields = length(dots.nDots); %number of dot fields
totalDots = sum(dots.nDots);
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

display.fixation = dots.center;
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
config.nTotalTrials = 10; % 10 dummay easy trials

result.response = NaN * ones(1, config.nTotalTrials);
config.coherence = [.9 .9 .8 .8 .7 .7 .6 .6 .5 .5];

% Results structure
result.response = NaN * ones(1,config.nTotalTrials);
result.keyResponse = NaN * ones(1,config.nTotalTrials);
config.randOrder = repmat([1 2], 1, 5);
config.randOrder = Shuffle(config.randOrder);
config.dir = repmat([0 1], 1, config.nTotalTrials/2);
config.dir = Shuffle(config.dir) .* 180;


scaleFactor = display.wRect(3)/size(bkIm,2);
srcRect = [0 0 size(bkIm,2)-1 size(bkIm,1)-1];
destRect = CenterRectOnPoint(ScaleRect(srcRect,scaleFactor,scaleFactor),display.cx,display.cy);

bkTexture = Screen('MakeTexture', display.wPtr, bkIm);

% Screen('DrawTexture', display.wPtr, bkTexture,srcRect,destRect);
Screen('Flip', display.wPtr);

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Show the instructions
instrIm = imread('Instructions.jpg');


scaleFactor = display.wRect(3)/size(instrIm,2);
srcRect = [0 0 size(instrIm,2)-1 size(instrIm,1)-1];
destRect = CenterRectOnPoint(ScaleRect(srcRect,scaleFactor,scaleFactor),display.cx,display.cy);
instTexture = Screen('MakeTexture', display.wPtr, instrIm);
Screen('DrawTexture', display.wPtr, instTexture,srcRect,destRect);
Screen('Flip', display.wPtr);    





wait4Space = 0;
while ~wait4Space
    [keyIsDown, secs, keyCode, deltaSecs] = KbCheck(-1);
    if keyIsDown && keyCode(kb.spaceKey)
        wait4Space = 1;
    end
end

% Display the instructions
str = 'Hit the space to continue';
strBounds = Screen('TextBounds', display.wPtr, str);
Screen('DrawTexture', display.wPtr, bkTexture,srcRect,destRect);
Screen('DrawText',display.wPtr, str, display.cx - round((strBounds(3)-strBounds(1))/2), ...
    display.cy,[1 1 1]);
Screen('Flip',display.wPtr);

WaitSecs(.5);
wait4Space = 0;
while ~wait4Space
    [keyIsDown, secs, keyCode, deltaSecs] = KbCheck(-1);
    if keyIsDown && keyCode(kb.spaceKey)
        wait4Space = 1;
    end
end


score = 0;
trial = 1;
%% Trials
while trial <= config.nTotalTrials

    dots.coherence = config.coherence(trial);

    dots = CreateDots(dots,config.dir(trial),display);
    
    Screen('DrawTexture', display.wPtr, bkTexture,srcRect,destRect);
    MakeFixation(display);
    Screen('Flip', display.wPtr);
    WaitSecs(.5);
    
    j = 1; t = GetSecs; isResponded = 0;
   % While the dots are on display and the keypress has not occurred
    while j <= dots.dur*display.frameRate && ~isResponded
        
        % Draw the dots on the screen
        m = dots.xy(:,:,j) * display.ppd;
        Screen('DrawDots', display.wPtr, m, dots.size, dots.dotColor, dots.center, 2);
        MakeFixation(display);
        Screen('Flip', display.wPtr);
        j = j + 1; % Advance frame number
        % Wait the interframe interval and check for keypresses
        
        [pressed, firstPress] = KbQueueCheck();
        if pressed % if key is down
            if strcmp(KbName(firstPress), 'DownArrow') 
                result.RT(trial) = GetSecs - t;
                result.keyResponse(trial) = 1;
                isResponded = 1;
            elseif strcmp(KbName(firstPress), 'RightArrow')
                result.RT(trial) = GetSecs - t;
                result.keyResponse(trial) = 0;
                isResponded = 1; 
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
    
    % Flip back to the target and wait for keypress
    MakeFixation(display);
    Screen('Flip', display.wPtr);
    
     while ~isResponded
        
        [pressed, firstPress] = KbQueueCheck();
        if pressed % if key is down
            if strcmp(KbName(firstPress), 'DownArrow')
                result.RT(trial) = GetSecs - t;
                result.keyResponse(trial) = 1;
                isResponded = 1;

            elseif strcmp(KbName(firstPress), 'RightArrow')
                result.RT(trial) = GetSecs - t;
                result.keyResponse(trial) = 0;
                isResponded = 1;

            elseif strcmp(KbName(firstPress), 'ESCAPE')
                Screen('CloseAll');
                ListenChar(0);
                ShowCursor;
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

    WaitSecs(1.5);

end


% Screen('DrawTexture', display.wPtr, bkTexture,srcRect,destRect);
% Screen('DrawText',display.wPtr, str5, display.cx - round((textBounds5(3)-textBounds5(1))/2), ...
%         display.cy,[0 0 0]);
% 
% Screen('Flip', display.wPtr);
% 
% wait4Space = 0;
% while ~wait4Space
%     [keyIsDown, secs, keyCode, deltaSecs] = KbCheck(-1);
%     if keyIsDown && keyCode(kb.spaceKey)
%         wait4Space = 1;
%     end
% end

Screen('CloseAll');
ShowCursor(display.wPtr);
ListenChar(0);
RestrictKeysForKbCheck([]); % Re-enable all keys

