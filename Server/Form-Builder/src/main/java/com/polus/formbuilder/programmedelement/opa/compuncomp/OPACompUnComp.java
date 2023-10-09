package com.polus.formbuilder.programmedelement.opa.compuncomp;

import org.springframework.stereotype.Service;

import com.polus.formbuilder.programmedelement.ProgrammedElement;
import com.polus.formbuilder.programmedelement.ProgrammedElementModel;
@Service
public class OPACompUnComp implements ProgrammedElement{

	@Override
	public ProgrammedElementModel getBlankResponse() {
		return OPACompUnCompResponseModel.builder()
		  .id(10001)
		  .opaId(101)
		  .personEntityId(1001)
		  .entityNumber("27")
		  .natureOfRelationship("Self")
		  .numberOfDays(10)
		  .build();
	}

	@Override
	public ProgrammedElementModel getResponse(ProgrammedElementModel request) {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public ProgrammedElementModel action(ProgrammedElementModel request) {
		// TODO Auto-generated method stub
		return null;
	}	
}
