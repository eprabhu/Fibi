package com.polus.service;

import com.polus.entity.Unit;
import com.polus.repository.UnitRepository;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

@Service
public class UnitServiceImpl implements UnitService{

    @Autowired
    private UnitRepository unitRepository;

    public Unit getRootUnit() {
        return unitRepository.findRootUnit();
    }
}
