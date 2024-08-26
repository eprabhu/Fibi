/* tslint:disable:no-unused-variable */
import { async, ComponentFixture, TestBed } from '@angular/core/testing';
import { By } from '@angular/platform-browser';
import { DebugElement } from '@angular/core';

import { EntitySubawardRiskComponent } from './entity-subaward-risk.component';

describe('EntitySubawardRiskComponent', () => {
    let component: EntitySubawardRiskComponent;
    let fixture: ComponentFixture<EntitySubawardRiskComponent>;

    beforeEach(async(() => {
        TestBed.configureTestingModule({
            declarations: [EntitySubawardRiskComponent]
        })
            .compileComponents();
    }));

    beforeEach(() => {
        fixture = TestBed.createComponent(EntitySubawardRiskComponent);
        component = fixture.componentInstance;
        fixture.detectChanges();
    });

    it('should create', () => {
        expect(component).toBeTruthy();
    });
});
