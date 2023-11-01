package com.polus.formbuilder.programmedelement.opa.compuncomp;

import java.util.List;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import com.polus.formbuilder.programmedelement.ProgrammedElement;
import com.polus.formbuilder.programmedelement.ProgrammedElementModel;
import com.polus.formbuilder.programmedelement.ProgrammedElementModuleDetails;
import com.polus.formbuilder.programmedelement.opa.common.OPACommonDAO;

@Component("OPACompUncompComponent")
public class OPACompUncomp implements ProgrammedElement {

	@Autowired
	private OPACompUncompDAO dao;

	@Autowired
	private OPACommonDAO commonDAO;

	@Override
	public ProgrammedElementModel getBlankResponse() {
		return null;
	}

	@Override
	public ProgrammedElementModel getResponse(ProgrammedElementModuleDetails moduleDetails,
			ProgrammedElementModel request) {
		OPACompUncompRequestModel opaRequest = (OPACompUncompRequestModel) request;
		if (opaRequest == null || opaRequest.getOpaDisclosureId() == null) {
			opaRequest = prepareOPARequest(moduleDetails);
		}

		try {

			commonDAO.personSyncOPAPersonEntityAction(opaRequest.getOpaDisclosureId(), opaRequest.getUpdateUser());

		} catch (Exception e) {
			System.out.println(e.getMessage());

		}

		List<OPACompUncompResponseDTO> disclosureActivity = dao.getPEComponentDetails(opaRequest);

		return OPACompUncompResponseModel.builder().data(disclosureActivity).build();
	}

	@Override
	public ProgrammedElementModel save(ProgrammedElementModuleDetails moduleDetails, ProgrammedElementModel request) {

		OPACompUncompRequestModel opaRequest = (OPACompUncompRequestModel) request;
		if (opaRequest == null || opaRequest.getOpaDisclosureId() == null) {
			opaRequest = prepareOPARequest(moduleDetails);
		}
		opaRequest.setUpdateUser(moduleDetails.getLoggedInUser());

		if ("SAVE".equals(opaRequest.getActionType())) {

			return OPACompUncompResponseModel.builder().data(dao.savePEComponent(opaRequest)).build();

		} else if ("DELETE".equals(opaRequest.getActionType())) {
			dao.deletePEComponent(opaRequest);

		}

		return null;
	}

	private OPACompUncompRequestModel prepareOPARequest(ProgrammedElementModuleDetails moduleDetails) {
		return OPACompUncompRequestModel.builder().opaDisclosureId(Integer.parseInt(moduleDetails.getModuleItemKey()))
				.updateUser(moduleDetails.getLoggedInUser()).build();
	}

}
