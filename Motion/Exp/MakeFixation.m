function MakeFixation(display, penWidth)

if ~exist('penWidth')
    penWidth = 2; 
end

switch display.fixShape
    case 'cross'
        Screen('DrawLine', display.wPtr, display.fixColor, display.fixation(1)-round(display.fixSize/2), display.fixation(2), ...
            display.fixation(1)+round(display.fixSize/2), display.fixation(2), penWidth);
        Screen('DrawLine', display.wPtr, display.fixColor, display.fixation(1), display.fixation(2)-round(display.fixSize/2), ...
            display.fixation(1), display.fixation(2)+round(display.fixSize/2), penWidth);
    case 'circle'
        rect = [display.fixation(1)-round(display.fixSize/2), display.fixation(2)-round(display.fixSize/2), ...
            display.fixation(1)+round(display.fixSize/2), display.fixation(2)+round(display.fixSize/2)];
        Screen('FillOval', display.wPtr, display.fixColor, rect);
    case 'bullseye'
        rect1 = [display.fixation(1)-round(display.fixSize/2), display.fixation(2)-round(display.fixSize/2), ...
            display.fixation(1)+round(display.fixSize/2), display.fixation(2)+round(display.fixSize/2)];
        rect2 = [display.fixation(1)-round(display.fixSize/3), display.fixation(2)-round(display.fixSize/3), ...
            display.fixation(1)+round(display.fixSize/3), display.fixation(2)+round(display.fixSize/3)];
        rect3 = [display.fixation(1)-round(display.fixSize/4), display.fixation(2)-round(display.fixSize/4), ...
            display.fixation(1)+round(display.fixSize/4), display.fixation(2)+round(display.fixSize/4)];
        rect4 = [display.fixation(1)-round(display.fixSize/10), display.fixation(2)-round(display.fixSize/10), ...
            display.fixation(1)+round(display.fixSize/10), display.fixation(2)+round(display.fixSize/10)];
        
        Screen('FillOval', display.wPtr, display.fixColor, rect1);
		Screen('FillOval', display.wPtr, [255 255 255], rect2);
		Screen('FillOval', display.wPtr, display.fixColor, rect3);
        
        Screen('DrawLine', display.wPtr, [128 128 128], display.fixation(1)-round(display.fixSize/2), display.fixation(2), ...
            display.fixation(1)+round(display.fixSize/2), display.fixation(2), penWidth);
        Screen('DrawLine', display.wPtr, [128 128 128], display.fixation(1), display.fixation(2)-round(display.fixSize/2), ...
            display.fixation(1), display.fixation(2)+round(display.fixSize/2), penWidth);
        Screen('FillOval', display.wPtr, [255 255 255], rect4);
    otherwise
        clc
        disp('Still to come...');
end