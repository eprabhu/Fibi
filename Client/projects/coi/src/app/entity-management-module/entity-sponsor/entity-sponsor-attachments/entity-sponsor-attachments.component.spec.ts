/* tslint:disable:no-unused-variable */
import { async, ComponentFixture, TestBed } from '@angular/core/testing';
import { By } from '@angular/platform-browser';
import { DebugElement } from '@angular/core';

import { EntitySponsorAttachmentsComponent } from './entity-sponsor-attachments.component';

describe('EntitySponsorAttachmentsComponent', () => {
    let component: EntitySponsorAttachmentsComponent;
    let fixture: ComponentFixture<EntitySponsorAttachmentsComponent>;

    beforeEach(async(() => {
        TestBed.configureTestingModule({
            declarations: [EntitySponsorAttachmentsComponent]
        })
            .compileComponents();
    }));

    beforeEach(() => {
        fixture = TestBed.createComponent(EntitySponsorAttachmentsComponent);
        component = fixture.componentInstance;
        fixture.detectChanges();
    });

    it('should create', () => {
        expect(component).toBeTruthy();
    });
});
