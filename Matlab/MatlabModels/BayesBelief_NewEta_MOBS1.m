% Bayes Belief Model from Barnby et al., (2022)

% DESCRIPTION HERE

%% Model
function [LL] = BayesBelief_NewEta_MOBS1(parms, data)

  % raw parameters

  pHI0_raw = parms(1);
  uHI0_raw = parms(2);
  pSI0_raw = parms(3);
  uSI0_raw = parms(4);
  upi_raw  = parms(5);
  w0_raw   = parms(6);
  wHI_raw  = parms(7);
  wSI_raw  = parms(8);

  pHI0     = 1./(1+exp(-pHI0_raw));
  uHI0     = exp(uHI0_raw);
  pSI0     = 1./(1+exp(-pSI0_raw));
  uSI0     = exp(uSI0_raw);
  upi      = 1./(1+exp(-upi_raw));
  w0       = w0_raw;
  whi      = 1./(1+exp(-wHI_raw));
  wsi      = 1./(1+exp(-wSI_raw));

  if length(parms) == 9
        eta_raw  = parms(9);
        eta      = 1./(1+exp(-eta_raw));
  else
        eta      = 1;
  end

  % initialise elements
  tn = 6;  % 6 trials
  phase=3;  % 3 blocks
  Nb = 9;   % resolution of matrices
  Na = 2;   % actual number of actions in expt.

  err      = 0.02/(Nb*Nb);

  Bin = Nb-1;

  MDFHI = binopdf(0:Bin,Bin,pHI0);
  MDFHI = MDFHI .^ (1/uHI0);
  MDFHI = MDFHI ./ sum(MDFHI);

  MDFSI = binopdf(0:Bin,Bin,pSI0);
  MDFSI = MDFSI .^ (1/uSI0);
  MDFSI = MDFSI ./ sum(MDFSI);

  pSIHI= (MDFSI)' .* MDFHI;   %pSIHI/sum(pSIHI(:));

  % Set up the map between 'attributes' and actions :
  pi  = zeros(Nb,Nb,Na);    % Possible policies for actual range of actions

  % pinit, pstep, bu, mu fine-tune the map ... see below.
  offs = (Nb+1)/2;
  for SI = 1:Nb
    for HI = 1:Nb
      x = w0 + (wsi * (SI-offs)) + (whi * (HI-offs));
      pi(SI,HI,1)  = 1./(1+exp(-x)) ; % prob. of unfair offer goes up w. HI, SI
      pi(SI,HI,2)  = 1 - pi(SI,HI,1);
    end
  end

  % Run the inference

    LL = 0;
    pri0 = pSIHI; % prior
    post = pri0; % this is the belief that will be updated with each trial

    % HI and SI attributions:
    ro = 1:(phase*tn); % rows of data matrix
    as = data(ro,1);  aind = round((Na-1)*(as+1));
    hi = data(ro,2);  hind = round((Nb-1)*(hi)+1);
    si = data(ro,3);  sind = round((Nb-1)*(si)+1);

     for t = 1:(tn*phase)  % loop

         if t == 7 || t == 13

            post = (pri0 .* (1-eta)) + (post .* eta);

         end

     pri = post;              % new prior is last posterior

     post = pi(:,:,aind(t)) .* pri;
     post = post/sum(post(:));  % Bayes

     pol  = post.^(1/upi);
     pol  = pol/sum(pol(:));

     pol  = (pol+err)./(1+err.*(length(pol(:))));
     lik  = pol(sind(t),hind(t))+eps; % add small noise floor
     LL   = LL + log(lik);         % accumulate sum log lik

     end
 end % end of function
