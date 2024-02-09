package com.polus.formbuilder.programmedelement.opa.outsidefinancialinterest;

import java.util.List;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import com.polus.formbuilder.programmedelement.ProgrammedElement;
import com.polus.formbuilder.programmedelement.ProgrammedElementModel;
import com.polus.formbuilder.programmedelement.ProgrammedElementModuleDetails;
import com.polus.formbuilder.programmedelement.opa.common.OPACommonDAO;

@Component("OPAOutsideFinancialRelationComponent")
public class OPAOutsideFinancialInterest implements ProgrammedElement {

	@Autowired
	private OPAOutsideFinancialInterestDAO dao;

	@Autowired
	private OPACommonDAO commonDAO;

	@Override
	public ProgrammedElementModel getBlankResponse() {
		return null;
	}

	@Override
	public ProgrammedElementModel getResponse(ProgrammedElementModuleDetails moduleDetails,
			ProgrammedElementModel request) throws InterruptedException {
		OPAOutsideFinancialInterestRequestModel opaRequest = (OPAOutsideFinancialInterestRequestModel) request;
		if (opaRequest == null || opaRequest.getOpaDisclosureId() == null) {
			opaRequest = prepareOPARequest(moduleDetails);
		}

		try {

			commonDAO.personSyncOPAPersonEntityAction(opaRequest.getOpaDisclosureId(), opaRequest.getUpdateUser());

		} catch (Exception e) {
			System.out.println(e.getMessage());

		}
		Thread.sleep(2000);
		List<OPAOutsideFinancialInterestResponseDTO> disclosureActivity = dao.getPEComponentDetails(opaRequest);

		return OPAOutsideFinancialInterestResponseModel.builder().data(disclosureActivity).build();
	}

	@Override
	public ProgrammedElementModel save(ProgrammedElementModuleDetails moduleDetails, ProgrammedElementModel request) {

		OPAOutsideFinancialInterestRequestModel opaRequest = (OPAOutsideFinancialInterestRequestModel) request;
		if (opaRequest == null || opaRequest.getOpaDisclosureId() == null) {
			opaRequest = prepareOPARequest(moduleDetails);
		}
		opaRequest.setUpdateUser(moduleDetails.getLoggedInUser());

		if ("SAVE".equals(opaRequest.getActionType())) {

			return OPAOutsideFinancialInterestResponseModel.builder().data(dao.savePEComponent(opaRequest)).build();

		} else if ("DELETE".equals(opaRequest.getActionType())) {
			dao.deletePEComponent(opaRequest);

		}

		return null;
	}

	private OPAOutsideFinancialInterestRequestModel prepareOPARequest(ProgrammedElementModuleDetails moduleDetails) {
		return OPAOutsideFinancialInterestRequestModel.builder().opaDisclosureId(Integer.parseInt(moduleDetails.getModuleItemKey()))
				.updateUser(moduleDetails.getLoggedInUser()).build();
	}

}
