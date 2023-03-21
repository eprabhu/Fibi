package com.polus.fibicomp.agreements.service;

import org.springframework.stereotype.Service;

import com.polus.fibicomp.agreements.vo.AgreementVO;

@Service
public interface AgreementCopyService {
	/**
	 * This method is used copy Agreement.
	 * @param agreementVO 
	 * @return String response.
	 */
	public AgreementVO copyAgreement(AgreementVO vo);
}
