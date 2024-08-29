/* tslint:disable:no-unused-variable */
import { async, ComponentFixture, TestBed } from '@angular/core/testing';
import { By } from '@angular/platform-browser';
import { DebugElement } from '@angular/core';

import { EntityComplianceAttachmentsComponent } from './entity-compliance-attachments.component';

describe('EntityComplianceAttachmentsComponent', () => {
    let component: EntityComplianceAttachmentsComponent;
    let fixture: ComponentFixture<EntityComplianceAttachmentsComponent>;

    beforeEach(async(() => {
        TestBed.configureTestingModule({
            declarations: [EntityComplianceAttachmentsComponent]
        })
            .compileComponents();
    }));

    beforeEach(() => {
        fixture = TestBed.createComponent(EntityComplianceAttachmentsComponent);
        component = fixture.componentInstance;
        fixture.detectChanges();
    });

    it('should create', () => {
        expect(component).toBeTruthy();
    });
});
