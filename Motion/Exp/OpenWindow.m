function display = OpenWindow(display)
%display = OpenWindow([display])
%
%Calls the psychtoolbox command "Screen('OpenWindow') using the 'display'
%structure convention.
%
%Inputs:
%   display             A structure containing display information with fields:
%       screenNum       Screen Number (default is 0)
%       bkColor         Background color (default is black: [0,0,0])
%       skipChecks      Flag for skpping screen synchronization (default is 0, or don't check)
%                       When set to 1, vbl sync check will be skipped,
%                       along with the text and annoying visual (!) warning
%
%Outputs:
%   display             Same structure, but with additional fields filled in:
%       windowPtr       Pointer to window, as returned by 'Screen'
%       frameRate       Frame rate in Hz, as determined by Screen('GetFlipInterval')
%       resolution      [width,height] of screen in pixels
%
%Note: for full functionality, the additional fields of 'display' should be
%filled in:
%
%       dist             distance of viewer from screen (cm)
%       width            width of screen (cm)

%Written 11/13/07 by gmb

if ~exist('display','var')
    display.screenNum = 0;
end

if ~isfield(display,'screenNum')
    display.screenNum = 0;
end

if ~isfield(display,'bkColor')
    display.bkColor = [0,0,0]; %black
end

if ~isfield(display,'skipChecks')
    display.skipChecks = 0;
end

if ~isfield(display,'stereoMode')
    display.stereoMode = 0;
end

if display.skipChecks  
    Screen('Preference', 'Verbosity', 0);
    Screen('Preference', 'SkipSyncTests',1);
    Screen('Preference', 'VisualDebugLevel',0);
end

PsychImaging('PrepareConfiguration');

if display.stereoMode == 8
    PsychImaging('AddTask', 'LeftView', 'DisplayColorCorrection', 'AnaglyphStereo');
    PsychImaging('AddTask', 'RightView', 'DisplayColorCorrection', 'AnaglyphStereo');
end
%Open the window
[display.wPtr, display.wRect]=PsychImaging('OpenWindow', display.screenNum, display.bkColor, [], [], [], display.stereoMode, 0);

% alpha-blending!!
[sourceFactorOld, destinationFactorOld]=Screen('BlendFunction', display.wPtr, GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);

%Set the display parameters 'frameRate' and 'resolution'
display.ifi = Screen('GetFlipInterval',display.wPtr);
display.frameRate = round(1/display.ifi); %Hz
display.resolution = display.wRect([3,4]);
[display.cx display.cy] = RectCenter(display.wRect);

if display.stereoMode == 4
    display.ppd = display.resolution(1)/display.widthInVisualAngle/2;
else
    display.ppd = display.resolution(1)/display.widthInVisualAngle;
end

% Screen('LoadNormalizedGammaTable', display.wPtr, display.gamma);