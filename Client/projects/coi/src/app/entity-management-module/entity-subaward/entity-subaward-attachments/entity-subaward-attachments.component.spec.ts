/* tslint:disable:no-unused-variable */
import { async, ComponentFixture, TestBed } from '@angular/core/testing';
import { By } from '@angular/platform-browser';
import { DebugElement } from '@angular/core';

import { EntitySubawardAttachmentsComponent } from './entity-subaward-attachments.component';

describe('EntitySubawardAttachmentsComponent', () => {
    let component: EntitySubawardAttachmentsComponent;
    let fixture: ComponentFixture<EntitySubawardAttachmentsComponent>;

    beforeEach(async(() => {
        TestBed.configureTestingModule({
            declarations: [EntitySubawardAttachmentsComponent]
        })
            .compileComponents();
    }));

    beforeEach(() => {
        fixture = TestBed.createComponent(EntitySubawardAttachmentsComponent);
        component = fixture.componentInstance;
        fixture.detectChanges();
    });

    it('should create', () => {
        expect(component).toBeTruthy();
    });
});
