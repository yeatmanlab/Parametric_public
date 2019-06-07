function dots = CreateDots(dots,dir,display)

dots.r = dots.rmax * sqrt(rand(1, dots.nDots));	% r
dots.r(dots.r<dots.rmin) = dots.rmin;
dots.t = 2*pi*rand(1, dots.nDots);                     % theta polar coordinate
dots.cs = [cos(dots.t); sin(dots.t)];
dots.xy(:,:,1) = [dots.r; dots.r] .* dots.cs;   % dot positions in Cartesian coordinates (pixels from center)

%displacement per frame (pixels)
dir = dir*ones(1,dots.nDots);
nRand= round(dots.nDots*(1-dots.coherence));
dir(1:nRand) = 360*rand(1,nRand);
dots.dxdy(:) = [dots.speed*cos(pi*dir/180)*display.ifi; ...
    dots.speed*sin(pi*dir/180)*display.ifi];

% dots.dotColor(:,id) = repmat(dots.color',1,dots.nDots(i)); % all black
dots.dotColor = [repmat(dots.color',1,dots.nDots/2) repmat([255 255 255]',1,dots.nDots/2)];
index = Shuffle(1:length(dots.dotColor));
dots.dotColor = dots.dotColor(:,index);

dots.dotAge = floor(dots.dotLife*rand(1,dots.nDots));
dots.dotSize = repmat(dots.size,1,dots.nDots);

for i = 2: dots.nFrames
    dots.xy(:,:,i) = dots.xy(:,:,i-1) + dots.dxdy;
    dots.dotAge = dots.dotAge+1;
    deadDots = dots.dotAge == dots.dotLife;
    
    r_out = find(dots.xy(1,:,i).^2 + dots.xy(2,:,i).^2 > dots.rmax^2 | ...
        dots.xy(1,:,i).^2 + dots.xy(2,:,i).^2 < dots.rmin^2 | deadDots);	% dots to reposition
    nout = length(r_out);
    if nout
        dots.r(r_out) = dots.rmax * sqrt(rand(1,nout));
        dots.r(dots.r<dots.rmin) = dots.rmin;
        dots.t(r_out) = 2*pi*(rand(1,nout));
        
        % now convert the polar coordinates to Cartesian
        dots.cs(:,r_out) = [cos(dots.t(r_out)); sin(dots.t(r_out))];
        dots.xy(:,r_out,i) = [dots.r(r_out); dots.r(r_out)] .* dots.cs(:,r_out);
    end
    
    %reposition and revive the dead dots
    dots.dotAge(deadDots) = 0;  %born again!
end