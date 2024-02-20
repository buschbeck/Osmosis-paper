% NORMALISE - Rescale image values to 0-1.
%
% Function to rescale elements of a matrix
% so that the minimum value is 0 and the 
% maximum value is 1.
%
% Usage: rescaled = normalise(im)
%

function n = normalise(a)

    if ~isa(a,'double'), a = double(a); end
    n = a - min(min(a));
    n = n/max(max(n));
    