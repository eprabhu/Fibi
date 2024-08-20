package com.polus.fibicomp.globalentity.service;

import java.util.List;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import com.polus.core.common.dao.CommonDao;
import com.polus.core.pojo.Currency;
import com.polus.fibicomp.globalentity.repository.GlobalEntityRepository;

@Service(value = "globalEntityService")
@Transactional
public class GlobalEntityServiceImpl implements GlobalEntityService {

	@Autowired
	private GlobalEntityRepository entityRepository;

	@Autowired
	private CommonDao commonDao;

	@Override
	public ResponseEntity<Boolean> isDunsNumberExists(String dunsNumber) {
		return new ResponseEntity<>(entityRepository.isDunsNumberExists(dunsNumber) > 0, HttpStatus.OK);
	}

	@Override
	public ResponseEntity<Boolean> isCageNumberExists(String cageNumber) {
		return new ResponseEntity<>(entityRepository.isCageNumberExists(cageNumber) > 0, HttpStatus.OK);
	}

	@Override
	public ResponseEntity<Boolean> isUeiNumberExists(String ueiNumber) {
		return new ResponseEntity<>(entityRepository.isUeiNumberExists(ueiNumber) > 0, HttpStatus.OK);
	}

	@Override
	public ResponseEntity<List<Currency>> fetchCurrencyDetails(){
		return new ResponseEntity<>(commonDao.fetchCurrencyDetails(), HttpStatus.OK);
	}

}
