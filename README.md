# DC Deoffset Nyquist plugin for Audacity
DC Deoffset is a fairly simple Nyquist plugin for Audacity for removing a DC offset from a signal.

The core of this plugin's DC offset removal is subtracting a constant value from an entire selection. No high-pass filter, no center-the-waveform-on-zero, just "hey, see this non-zero horizontal line over here? move it (and/or some other stuff) so that the line is (or would be) at zero."

Its intended usage is similar to the built-in Noise Reduction plugin:
1. Select some part of the signal that's effectively silent, but has a DC offset you want to correct for.
2. Run this plugin with the "Analyze selection" action selected to calculate the DC offset level.
3. Select the part of the signal you want to correct the DC offset in.
4. Run the plugin again, but with the "Apply correction from analysis" action selected in order to remove the previously calculated offset from the selection.

There are also sliders and an "Apply correction from sliders" action if you want to select the offset level manually instead. 

The functionality this plugin provides may already be in Audacity or one of its many included plugins. I just had a specific thing I wanted done, and playing around with writing my own plugin was more appealing than searching for how to do it with what's already out there.

# License
This software is released under the terms of the MIT License.
