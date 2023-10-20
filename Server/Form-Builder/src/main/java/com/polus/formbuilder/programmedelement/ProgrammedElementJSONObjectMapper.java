package com.polus.formbuilder.programmedelement;

import org.springframework.stereotype.Component;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.JsonMappingException;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.polus.formbuilder.programmedelement.opa.compuncomp.OPACompUncompRequestModel;

@Component
public class ProgrammedElementJSONObjectMapper {

	public ProgrammedElementModel jsonObjectMapper( String programElementName,
													String type,
													String programmedElementJson) {
		
		ObjectMapper objectMapper = new ObjectMapper(); 
		ProgrammedElementModel programmedElement = null;
		try {
			
			if("OPACompUncompComponent".equals(programElementName)) {
				programmedElement = (ProgrammedElementModel) objectMapper.readValue(programmedElementJson, OPACompUncompRequestModel.class);
			}
			
		} catch (JsonMappingException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		} catch (JsonProcessingException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
		return programmedElement;
		
	}
	
	
}
