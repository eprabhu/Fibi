import { NgModule } from '@angular/core';
import { CommonModule } from '@angular/common';
import { HeaderSlidersComponent } from './header-sliders.component';
import { SharedModule } from '../../shared/shared.module';
import { SharedComponentModule } from '../../shared-components/shared-component.module';
import { SfiModule } from '../sfi/sfi.module';
import { NotesAttachmentsModule } from '../../user-dashboard/notes/notes-attachments.module';
import { AttachmentsModule } from '../../user-dashboard/attachments/attachments.module';

@NgModule({
    imports: [
        CommonModule,
        SharedModule,
        SharedComponentModule,
        SfiModule,
        NotesAttachmentsModule,
        AttachmentsModule
    ],
    declarations: [HeaderSlidersComponent],
    exports: [HeaderSlidersComponent]
})
export class HeaderSlidersModule { }
