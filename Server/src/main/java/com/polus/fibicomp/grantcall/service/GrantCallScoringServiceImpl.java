package com.polus.fibicomp.grantcall.service;

import java.util.List;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import com.polus.fibicomp.committee.dao.CommitteeDao;
import com.polus.fibicomp.grantcall.dao.GrantCallScoringDao;
import com.polus.fibicomp.grantcall.pojo.GrantCallScoringCriteria;
import com.polus.fibicomp.grantcall.vo.GrantCallScoringVO;

@Transactional
@Service(value = "grantCallScoringService")
public class GrantCallScoringServiceImpl implements GrantCallScoringService {

	protected static Logger logger = LogManager.getLogger(GrantCallScoringServiceImpl.class.getName());

	@Autowired
	private GrantCallScoringDao grantCallScoringDao;

	@Autowired
	private CommitteeDao committeeDao;

	@Override
	public String fetchAllScoringCriteria(GrantCallScoringVO vo) {
		vo.setScoringCriteria(grantCallScoringDao.fetchAllScoringCriteria());
		vo.setGrantCallScoringCriterias(grantCallScoringDao.fetchScoringCriteriaGrantCallId(vo.getGrantCallId()));
		return committeeDao.convertObjectToJSON(vo);
	}

	@Override
	public String saveOrUpdateGrantCallScoringCriteria(GrantCallScoringVO vo) {
		List<GrantCallScoringCriteria> scoringCriterias=vo.getGrantCallScoringCriterias();
		for(GrantCallScoringCriteria scoringCriteria : scoringCriterias) {
			grantCallScoringDao.saveOrUpdateGrantCallScoringCriteria(scoringCriteria);
		}
		return committeeDao.convertObjectToJSON(vo);	
	}

	@Override
	public String deleteGrantCallScoringCriteria(GrantCallScoringVO vo) {
		String successMessage = grantCallScoringDao.deleteGrantCallScoringCriteria(vo.getGrantScoringCriteriaId());
		vo.setMessage(successMessage);
		return committeeDao.convertObjectToJSON(vo);
	}

}
