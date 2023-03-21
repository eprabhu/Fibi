package com.polus.fibicomp.budget.service;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

@Transactional
@Service(value = "awardBudgetCalculationService")
public class AwardBudgetCalculationServiceImpl implements AwardBudgetCalculationService {

	protected static Logger logger = LogManager.getLogger(AwardBudgetCalculationServiceImpl.class.getName());

}
