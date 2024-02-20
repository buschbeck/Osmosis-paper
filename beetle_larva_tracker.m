
threshold = .05;
[name,path] = uigetfile('*.avi','select mp4 file to analyze','C:\Users\JEL\Documents\MATLAB\*.avi');
mov = VideoReader(name);
NumFrames = mov.NumFrames;
FrameRate = mov.FrameRate;

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Make background to subtract from each image using 'MakeBackground'.
INT = round(2*FrameRate);
VidLength = 1;
counter = 1;
fid = fopen([name(1:end-4),'_BkGrd.tif']);
if fid > 0
    fclose(fid);
    BkGrd = imread([name(1:end-4),'_BkGrd.tif']);
    BkGrd = double(BkGrd)/255;
else
    disp('Please wait while I generate a background image to subtract');
    for i = 1:INT:(round(NumFrames/VidLength))
    ['I am adding background frame ',num2str(counter),' out of ',num2str(round(NumFrames/VidLength/INT))]
    counter = counter+1;
    pic = read(mov,i);
    pic = double(pic(:,:,3))/255;
        if i == 1
            BkGrd = pic;
        else
            BkGrd = pic+BkGrd;
        end;
    end;
    BkGrd = BkGrd/counter;
    imwrite(BkGrd,[path,name(1:end-4),'_BkGrd.tif'],'TIFF');
end;
counter = 1;
for i = 1:FrameRate:NumFrames
    ['I am adding analyzing frame ',num2str(counter),' out of ',num2str(NumFrames)];
    pic = read(mov,i);
    pic = double(pic(:,:,3))/255;
    pic = pic - BkGrd;
    pic = normalise(pic.^2);
    x = 1;
    while x > .005
        threshold = threshold + .0001;
        f = find(pic > threshold);
        x = length(f)/length(pic(:));
    end
    pic = pic*0;
    pic(f) = 1;
    pic = logical(pic);
    pic = bwareafilt(pic,1);
    larva_features = detectSURFFeatures(pic);
%     larva_features = detectHarrisFeatures(pic);
    cla
    imshow(pic);
    hold on; plot(larva_features.selectStrongest(1));
    hold off
    title(['Frame ',num2str(counter),' which is ',num2str(i),' out of ',num2str(NumFrames)])
    pause(.1)
    data(counter,:)= [larva_features.Location(1,1) larva_features.Location(1,2)];
    counter = counter +1;
end;

return


figure;
imshow(pic);
hold on;plot(larva_features.Location(1,1),larva_features.Location(1,2),'r*')
hold off
title('SURF features in larva frame')

% make background
% subtract background
% square image
% normalise image
% find greater than threshold
% f = find(pic > THRESHOLD); pic(f) = 1; pic(~f) = 0;
% find features