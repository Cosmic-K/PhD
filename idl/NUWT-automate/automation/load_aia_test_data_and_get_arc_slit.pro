print, 'Restoring example AIA 171 north pole cutout data ...'
restore, '/usr/users/UNN/jcsj7/Research/data/testing/ex_aia_171_lv_1.0_north_pole_cutout.sav'
restore, '/usr/users/UNN/jcsj7/Research/data/testing/ex_aia_171_lv_1.5_north_pole_cutout.sav'

print, 'Running arc_slit.pro to create td array using the following parameters:'
print, '   center=[255.5, -1279.5], radius=1617.0
arc_slit, ex_lv15_data, td, center=[255.5, -1279.5], radius=1617.0
