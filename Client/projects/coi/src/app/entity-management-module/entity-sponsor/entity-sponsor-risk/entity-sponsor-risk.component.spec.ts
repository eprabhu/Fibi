/* tslint:disable:no-unused-variable */
import { async, ComponentFixture, TestBed } from '@angular/core/testing';
import { By } from '@angular/platform-browser';
import { DebugElement } from '@angular/core';

import { EntitySponsorRiskComponent } from './entity-sponsor-risk.component';

describe('EntitySponsorRiskComponent', () => {
    let component: EntitySponsorRiskComponent;
    let fixture: ComponentFixture<EntitySponsorRiskComponent>;

    beforeEach(async(() => {
        TestBed.configureTestingModule({
            declarations: [EntitySponsorRiskComponent]
        })
            .compileComponents();
    }));

    beforeEach(() => {
        fixture = TestBed.createComponent(EntitySponsorRiskComponent);
        component = fixture.componentInstance;
        fixture.detectChanges();
    });

    it('should create', () => {
        expect(component).toBeTruthy();
    });
});
