package com.polus.formbuilder.programmedelement.opa.instituteresourceuse;

import java.util.List;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import com.polus.formbuilder.programmedelement.ProgrammedElement;
import com.polus.formbuilder.programmedelement.ProgrammedElementModel;
import com.polus.formbuilder.programmedelement.ProgrammedElementModuleDetails;
import com.polus.formbuilder.programmedelement.opa.common.OPACommonDAO;

@Component("OPAInstituteResourceUseComponent")
public class OPAInstituteResourceUse implements ProgrammedElement {

	@Autowired
	private OPAInstituteResourceUseDAO dao;

	@Autowired
	private OPACommonDAO commonDAO;

	@Override
	public ProgrammedElementModel getBlankResponse() {
		return null;
	}

	@Override
	public ProgrammedElementModel getResponse(ProgrammedElementModuleDetails moduleDetails, ProgrammedElementModel request) {
		OPAInstituteResourceUseRequestModel opaRequest = (OPAInstituteResourceUseRequestModel) request;
		if (opaRequest == null || opaRequest.getOpaDisclosureId() == null) {
			opaRequest = prepareOPARequest(moduleDetails);
		}

		try {

			commonDAO.personSyncOPAPersonEntityAction(opaRequest.getOpaDisclosureId(), opaRequest.getUpdateUser());

		} catch (Exception e) {
			System.out.println(e.getMessage());

		}

		List<OPAInstituteResourceUseResponseDTO> lsInstituteResourceUse = dao.getPEComponentDetails(opaRequest);

		return OPAInstituteResourceUseResponseModel.builder().data(lsInstituteResourceUse).build();
	}

	private OPAInstituteResourceUseRequestModel prepareOPARequest(ProgrammedElementModuleDetails moduleDetails) {
		return OPAInstituteResourceUseRequestModel.builder().opaDisclosureId(Integer.parseInt(moduleDetails.getModuleItemKey()))
				.updateUser(moduleDetails.getLoggedInUser()).build();
	}

	@Override
	public ProgrammedElementModel save(ProgrammedElementModuleDetails moduleDetails, ProgrammedElementModel request) {
		OPAInstituteResourceUseRequestModel opaRequest = (OPAInstituteResourceUseRequestModel) request;
		if (opaRequest == null || opaRequest.getOpaDisclosureId() == null) {
			opaRequest = prepareOPARequest(moduleDetails);
		}
		opaRequest.setUpdateUser(moduleDetails.getLoggedInUser());

		if ("SAVE".equals(opaRequest.getActionType())) {

			return OPAInstituteResourceUseResponseModel.builder().data(dao.savePEComponent(opaRequest)).build();

		} else if ("DELETE".equals(opaRequest.getActionType())) {
			dao.deletePEComponent(opaRequest);

		}

		return null;
	}

	

}
