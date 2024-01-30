package com.polus.formbuilder.programmedelement;

import org.springframework.stereotype.Component;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.JsonMappingException;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.polus.formbuilder.programmedelement.opa.compuncomp.OPACompUncompRequestModel;
import com.polus.formbuilder.programmedelement.opa.instituteresourceuse.OPAInstituteResourceUseRequestModel;
import com.polus.formbuilder.programmedelement.opa.outsidefinancialinterest.OPAOutsideFinancialInterestRequestModel;
import com.polus.formbuilder.programmedelement.opa.studentsubordinateinvolvement.OPAStudentSubordinateInvolvementRequestModel;

@Component
public class ProgrammedElementJSONObjectMapper {

	public ProgrammedElementModel jsonObjectMapper( String programElementName,
													String type,
													String programmedElementJson) {
		
		ObjectMapper objectMapper = new ObjectMapper(); 
		ProgrammedElementModel programmedElement = null;
		try {
			if ("OPACompUncompComponent".equals(programElementName)) {
				programmedElement = objectMapper.readValue(programmedElementJson, OPACompUncompRequestModel.class);
			} else if ("OPAOutsideFinancialRelationComponent".equals(programElementName)) {
				programmedElement = objectMapper.readValue(programmedElementJson, OPAOutsideFinancialInterestRequestModel.class);
			} else if ("OPAStudentSubordinateInvolvementComponent".equals(programElementName)) {
				programmedElement = objectMapper.readValue(programmedElementJson, OPAStudentSubordinateInvolvementRequestModel.class);
			} else if ("OPAInstituteResourceUseComponent".equals(programElementName)) {
				programmedElement = objectMapper.readValue(programmedElementJson, OPAInstituteResourceUseRequestModel.class);
			}
		} catch (JsonMappingException e) {
			e.printStackTrace();
		} catch (JsonProcessingException e) {
			e.printStackTrace();
		}
		return programmedElement;
		
	}
	
	
}
