package com.polus.fibicomp.grantcall.service;

import java.util.ArrayList;
import java.util.List;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import com.polus.fibicomp.committee.dao.CommitteeDao;
import com.polus.fibicomp.common.dao.CommonDao;
import com.polus.fibicomp.grantcall.dao.GrantCallKPIDao;
import com.polus.fibicomp.grantcall.pojo.GrantCallKPI;
import com.polus.fibicomp.grantcall.pojo.GrantCallKPICriteria;
import com.polus.fibicomp.grantcall.vo.GrantCallKPIVO;

@Transactional
@Service(value = "grantCallKPIService")
public class GrantCallKPIServiceImpl implements GrantCallKPIService {

	protected static Logger logger = LogManager.getLogger(GrantCallKPIServiceImpl.class.getName());

	@Autowired
	private GrantCallKPIDao grantCallKPIDao;

	@Autowired
	private CommonDao commonDao;

	@Override
	public String getKPIByGrantCall(GrantCallKPIVO vo) {
		vo.setKpiTypes(grantCallKPIDao.fetchAllKPIs());
		vo.setGrantCallKpis(grantCallKPIDao.fetchKPIByGrantCallId(vo.getGrantCallId()));
		return commonDao.convertObjectToJSON(vo);
	}

	@Override
	public String saveUpdateGrantCallKPI(GrantCallKPIVO vo) {
		List<GrantCallKPI> grantCallKpis = vo.getGrantCallKpis();
		grantCallKpis.forEach(grantCallKPI -> grantCallKPIDao.saveOrUpdateGrantCallKPI(grantCallKPI));
		vo.setGrantCallKpis(grantCallKPIDao.fetchKPIByGrantCallId(vo.getGrantCallId()));
		return commonDao.convertObjectToJSON(vo);	
	}

	@Override
	public String deleteGrantCallKPI(GrantCallKPIVO vo) {
		vo.setMessage(grantCallKPIDao.deleteGrantCallKPI(vo.getGrantCallId(),vo.getGrantCallKpiId(),vo.getGrantCallKpiCriteriaId()));
		return commonDao.convertObjectToJSON(vo);
	}
}
