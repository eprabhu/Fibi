package com.polus.fibicomp.agreements.service;

import org.springframework.stereotype.Service;

import com.polus.fibicomp.agreements.vo.AgreementVO;

@Service
public interface AgreementWorkflowService {

	public void canTakeRoutingAction(AgreementVO agreementVO);

}
