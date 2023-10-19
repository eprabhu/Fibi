package com.polus.formbuilder.programmedelement.opa.compuncomp;

import java.util.List;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import com.polus.formbuilder.programmedelement.ProgrammedElement;
import com.polus.formbuilder.programmedelement.ProgrammedElementModel;
import com.polus.formbuilder.programmedelement.ProgrammedElementModuleDetails;

@Component("OPACompUncompComponent")
public class OPACompUnComp implements ProgrammedElement{

	@Autowired
	private OPACompUnCompDAO dao;
	
	@Override
	public ProgrammedElementModel getBlankResponse() {
		return null;
	}

	@Override
	public ProgrammedElementModel getResponse(ProgrammedElementModuleDetails moduleDetails,
											  ProgrammedElementModel request) {		
		 OPACompUnCompRequestModel opaRequest = (OPACompUnCompRequestModel) request;
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
		 
		 
		 
		 List<OPACompUnCompResponseDTO> disclosureActivity = dao.getDisclosureActivity(opaRequest);	
		 
		 return OPACompUnCompResponseModel.builder()
				 						  .data(disclosureActivity)
				 						  .build();
	}



	@Override
	public ProgrammedElementModel save(
										  ProgrammedElementModuleDetails moduleDetails,
										  ProgrammedElementModel request) {
		
		OPACompUnCompRequestModel opaRequest = (OPACompUnCompRequestModel) request;
		 if(opaRequest == null || opaRequest.getOpaDisclosureId() == null) {
			 opaRequest = prepareOPARequest(moduleDetails);
		 }
		 opaRequest.setUpdateUser(moduleDetails.getLoggedInUser());
		 
		if("SAVE".equals(opaRequest.getActionType())) {
			
			 return OPACompUnCompResponseModel.builder()
					  .data(dao.saveDisclosureActivity(opaRequest))
					  .build();	
			 
		}else if("DELETE".equals(opaRequest.getActionType())) {
			dao.deleteDisclosureActivity(opaRequest);
			
		}
		
		
		return null;
	}	

	private OPACompUnCompRequestModel prepareOPARequest(ProgrammedElementModuleDetails moduleDetails) {
		return OPACompUnCompRequestModel.builder()
										.opaDisclosureId(Integer.parseInt(moduleDetails.getModuleItemKey()))
										.updateUser(moduleDetails.getLoggedInUser())
										.build();
	}
	
}
