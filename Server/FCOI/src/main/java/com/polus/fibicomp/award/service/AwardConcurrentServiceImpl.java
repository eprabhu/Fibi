package com.polus.fibicomp.award.service;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import com.polus.fibicomp.award.vo.AwardVO;
import com.polus.fibicomp.common.dao.CommonDao;
import com.polus.fibicomp.constants.Constants;
import com.polus.fibicomp.manpowerintegration.scheduler.ManpowerIntegrationSchedulerService;
import com.polus.fibicomp.security.AuthenticatedUser;

@Service(value = "awardConcurrentService")
public class AwardConcurrentServiceImpl implements AwardConcurrentService {

	protected static Logger logger = LogManager.getLogger(AwardConcurrentServiceImpl.class.getName());

	@Autowired
	private AwardService awardService;

	@Autowired
	private ManpowerIntegrationSchedulerService manpowerIntegrationSchedulerService;

	@Autowired
	private CommonDao commonDao;

	@Override
	public String getAwardDetails(AwardVO awardVo) throws Exception {
		if (Boolean.TRUE.equals(awardVo.getIsMasterAwardCreation())) {
			AwardVO vo = new AwardVO();
			vo.setAwardNumber(awardVo.getAwardNumber());
			vo.setPersonId(AuthenticatedUser.getLoginPersonId());
			if (commonDao.getParameterValueAsBoolean(Constants.IS_MANPOWER_ENABLED) && Boolean.TRUE.equals(awardVo.getIsManpowerIntegrationRequired())) {
				logger.info("Thread for integrate manpower");
				manpowerIntegrationSchedulerService.threadForFetchAndIntegrateManpower(vo.getAwardNumber());
			}
			return awardService.getAwardDetails(vo);
		} else {
			return awardService.getAwardDetails(awardVo);
		}
	}

}
