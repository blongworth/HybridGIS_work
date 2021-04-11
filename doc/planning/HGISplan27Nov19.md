---
title: HGIS status and plan
author: Brett Longworth
date: 2017-10-27
---


# Status update

## Flow tests with glass and PEEK capillaries.

Tested for flow and discharge with various capillaries. No evidence of
discharge with pure CO2. Did not run source with beam. Looks like 2m of 75um
capillary should give good flows @ 15kPa (150mbar). 100um capillaries should
work well for lower pressures or with Helium dilution. PEEK capillaries seem
to produce lower flow rates than they should. May be getting pinched in 1/16
swage fittings.

## Low pressure source test

Ran source with dual inlet. Used 100um PEEK capillary. Delivered gas at pressure range from 0 to 800 mbar. No sparking, best currents (9.5uA) at 600 mbar. Given past performance this confirms that PEEK capillary is getting pinched. Glass capillaries or better PEEK fittings may be needed. 

## dual inlet depletion test

Tested how delivery pressure/flow depletes with bellows and micro-inlet. Used 100um PEEK capillary and leak checker. No measurable depletion over 30min with bellows at 25 mbar. Micro-inlet depletes by about a factor of 3 over 30 min starting at 25 mbar. 

## Dual bellows volume tests

Al and Kalina measured volumes in the dual inlet to help quantify ranges of sample size and delivery pressures. 

With CO2 pressure set at ~100 mbar...

* Largest sample volume: 30.75 mL, 122umol CO2
* Smallest volume: 0.43 mL, 1.8 umol CO2
* Sample bellows volume: 25.2 mL, 106 umol CO2
* Sample bellows compressed volume: 6.1 mL, 25.9 umol CO2
* Ref bellows volume: 100 mL

With the ability to adjust delivery pressure using capillary length, moles of CO2 required can go up or down by an order of magnitude. So:

At 100 mbar:

* micro inlet (CF): 1.8 umol
* smallest bellows: 28 umol
* largest bellows: 108 umol

At 10 mbar

* micro inlet (CF): 0.2 umol
* smallest bellows: 3 umol
* largest bellows: 11 umol

## New motor controllers

Installed. Need to be tested and code written.

## Gate valve installed

Gate valve installed. Needs control wiring and integration. Need to install pump and test.

# Next steps

Next test should probably be more ion source tests with low pressure capillaries. We should test delivery at 10 mbar and take live/dead gas data for various configurations.

On the Helium front: install turbopump, test operation. Initial tests of He mixing and delivery.

## Gas panel

* Leak check
* Test

## Helium dillution

* Install pump
* Test using gas panel

## Open split

* Pick capillaries
* Test with bottle gas
* Test with Gasbench

## Dual Bellows

### Labview

* Controls for bellows
* Integrate bellows & valve control

### Hardware

* HV gauge
* Better mbar range gauge
* Wire bellows motors
* Tube cracker
* Cold finger
* Helium dilution


### Planning

* Initial sample size
* Best operating pressure
* Helium dilution

### Tests

* Best flow rate
* Cracker test


