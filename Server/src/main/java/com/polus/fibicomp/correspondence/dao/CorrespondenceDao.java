package com.polus.fibicomp.correspondence.dao;

import com.polus.fibicomp.correspondence.dto.CorrespondenceDataBus;
import com.polus.fibicomp.correspondence.dto.IRBCorrespondenceDto;

public interface CorrespondenceDao {

	byte[] getTemplateData(CorrespondenceDataBus correspondenceDataBus);

	IRBCorrespondenceDto fetchIRBCorrespondenceData(CorrespondenceDataBus correspondenceDataBus);

	byte[] mergePlaceHolders(String outputDataFormat, byte[] data, IRBCorrespondenceDto irbCorrespondenceDto);

}
