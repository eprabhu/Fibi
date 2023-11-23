import { NgModule } from '@angular/core';
import { CommonModule } from '@angular/common';
import { AttachmentsComponent } from './attachments.component';
import { RouterModule, Routes } from '@angular/router';
import { FormsModule } from '@angular/forms';
import { SharedModule } from '../../shared/shared.module';
import { AttachmentsService } from './attachments.service';
import { MatIconModule } from '@angular/material/icon';
import { MatMenuModule } from '@angular/material/menu';
import { SharedComponentModule } from '../../shared-components/shared-component.module';

const routes: Routes = [{ path: '', component: AttachmentsComponent }];

@NgModule({
    imports: [
        CommonModule,
        FormsModule,
        SharedModule,
        RouterModule.forChild(routes),
        MatIconModule,
        MatMenuModule,
        SharedComponentModule
    ],
    declarations: [AttachmentsComponent],
    exports: [AttachmentsComponent],
    providers: [AttachmentsService]
})
export class AttachmentsModule { }
