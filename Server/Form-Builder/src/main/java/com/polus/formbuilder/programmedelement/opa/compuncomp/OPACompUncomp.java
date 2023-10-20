package com.polus.formbuilder.programmedelement.opa.compuncomp;

import java.util.List;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import com.polus.formbuilder.programmedelement.ProgrammedElement;
import com.polus.formbuilder.programmedelement.ProgrammedElementModel;
import com.polus.formbuilder.programmedelement.ProgrammedElementModuleDetails;

@Component("OPACompUncompComponent")
public class OPACompUncomp implements ProgrammedElement{

	@Autowired
	private OPACompUncompDAO dao;
	
	@Override
	public ProgrammedElementModel getBlankResponse() {
		return null;
	}

	@Override
	public ProgrammedElementModel getResponse(ProgrammedElementModuleDetails moduleDetails,
											  ProgrammedElementModel request) {		
		 OPACompUncompRequestModel opaRequest = (OPACompUncompRequestModel) request;
		 if(opaRequest == null || opaRequest.getOpaDisclosureId() == null) {
			 opaRequest = prepareOPARequest(moduleDetails);
		 }
		 
		 try {
			// personEntityRepository.syncOPAPerEntity(opaRequest.getOpaDisclosureId(), 
			//			 opaRequest.getUpdateUser());
			 
			 dao.personSyncOPAPersonEntityAction(opaRequest.getOpaDisclosureId(), 
						 opaRequest.getUpdateUser());
			 
			} catch (Exception e) {
			        System.out.println(e.getMessage());
			        
			         
			}
		 
		 
		 
		 List<OPACompUncompResponseDTO> disclosureActivity = dao.getDisclosureActivity(opaRequest);	
		 
		 return OPACompUncompResponseModel.builder()
				 						  .data(disclosureActivity)
				 						  .build();
	}



	@Override
	public ProgrammedElementModel save(
										  ProgrammedElementModuleDetails moduleDetails,
										  ProgrammedElementModel request) {
		
		OPACompUncompRequestModel opaRequest = (OPACompUncompRequestModel) request;
		 if(opaRequest == null || opaRequest.getOpaDisclosureId() == null) {
			 opaRequest = prepareOPARequest(moduleDetails);
		 }
		 opaRequest.setUpdateUser(moduleDetails.getLoggedInUser());
		 
		if("SAVE".equals(opaRequest.getActionType())) {
			
			 return OPACompUncompResponseModel.builder()
					  .data(dao.saveDisclosureActivity(opaRequest))
					  .build();	
			 
		}else if("DELETE".equals(opaRequest.getActionType())) {
			dao.deleteDisclosureActivity(opaRequest);
			
		}
		
		
		return null;
	}	

	private OPACompUncompRequestModel prepareOPARequest(ProgrammedElementModuleDetails moduleDetails) {
		return OPACompUncompRequestModel.builder()
										.opaDisclosureId(Integer.parseInt(moduleDetails.getModuleItemKey()))
										.updateUser(moduleDetails.getLoggedInUser())
										.build();
	}
	
	
	
	
	
}
