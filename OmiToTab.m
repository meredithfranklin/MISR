
%%
function OmiToTab(in_file, out_file)    
    % read geolocation fields;
    latitude = hdf5read(in_file, '/HDFEOS/SWATHS/ColumnAmountNO2/Geolocation Fields/Latitude');
    longitude = hdf5read(in_file, '/HDFEOS/SWATHS/ColumnAmountNO2/Geolocation Fields/Longitude');
    time = hdf5read(in_file, '/HDFEOS/SWATHS/ColumnAmountNO2/Geolocation Fields/Time');
    SolarZenithAngle = hdf5read(in_file, '/HDFEOS/SWATHS/ColumnAmountNO2/Geolocation Fields/SolarZenithAngle');
    
    % read data fields;
    CloudFraction = hdf5read(in_file, '/HDFEOS/SWATHS/ColumnAmountNO2/Data Fields/CloudFraction');
    CloudPressure = hdf5read(in_file, '/HDFEOS/SWATHS/ColumnAmountNO2/Data Fields/CloudPressure');
%   ColumnAmountNO2 = hdf5read(in_file, '/HDFEOS/SWATHS/ColumnAmountNO2/Data Fields/ColumnAmountNO2');
    ColumnAmountNO2Trop = hdf5read(in_file, '/HDFEOS/SWATHS/ColumnAmountNO2/Data Fields/ColumnAmountNO2Trop');
    XFlag = hdf5read(in_file, '/HDFEOS/SWATHS/ColumnAmountNO2/Data Fields/XTrackQualityFlags');
    vcdFlag = hdf5read(in_file, '/HDFEOS/SWATHS/ColumnAmountNO2/Data Fields/vcdQualityFlags');

    ColumnAmountNO2Trop(ColumnAmountNO2Trop <= -1.2676506E30) = NaN;
    CloudFraction(CloudFraction == -32767) = NaN;
    CloudFraction = double(CloudFraction) / 1000.0;
    CloudPressure(CloudPressure == -32767) == NaN;
    XFlag = uint16(XFlag);
    vcdFlag = uint16(vcdFlag);
    % print to out_file;    
    out_id = fopen(out_file, 'w');
    dim = size(latitude);
    
    fprintf(out_id, 'Latitude\tLongitude\tCloudPressure\tCloudFraction\tColumnAmountNO2Trop\tSolarZenithAngle\tFOVnumber\tTime\n');
    for i = 1:dim(2)
        for j = 1:dim(1)
            if (XFlag(j,i)==0 & bitget(vcdFlag(j,i),1)==0 & bitget(vcdFlag(j,i),5)==0) == 1
				fprintf(out_id, '%f\t%f\t%d\t%f\t%f\t%f\t%d\t%f\n', latitude(j,i), longitude(j,i), CloudPressure(j,i), CloudFraction(j,i), ColumnAmountNO2Trop(j,i), SolarZenithAngle(j,i), j, time(i));
            end
        end
    end
    fclose(out_id);