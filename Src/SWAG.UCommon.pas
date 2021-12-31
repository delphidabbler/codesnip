{
 * This Source Code Form is subject to the terms of the Mozilla Public License,
 * v. 2.0. If a copy of the MPL was not distributed with this file, You can
 * obtain one at https://mozilla.org/MPL/2.0/
 *
 * Copyright (C) 2013-2021, Peter Johnson (gravatar.com/delphidabbler).
 *
 * Defines records that encapsulate SWAG database categories and packets.
}


unit SWAG.UCommon;


interface


type
  ///  <summary>Record that encapsulates the data of a SWAG category.</summary>
  TSWAGCategory = record
    ///  <summary>Number that uniquely identifies a SWAG category.</summary>
    ID: Cardinal;
    ///  <summary>SWAG category title.</summary>
    Title: string;
  end;

type
  ///  <summary>Record that encapsulates the data that defines a SWAG packet
  ///  </summary>
  TSWAGPacket = record
    ///  <summary>Number that uniquely identifies a SWAG packet.</summary>
    ID: Cardinal;
    ///  <summary>ID of SWAG category that packet belongs to.</summary>
    Category: Cardinal;
    ///  <summary>File name of packet in original SWAG archive.</summary>
    FileName: string;
    ///  <summary>Date and time packet was added to or updated in the SWAG
    ///  archive.</summary>
    DateStamp: TDateTime;
    ///  <summary>SWAG packet title.</summary>
    Title: string;
    ///  <summary>Name of author(s) of SWAG packet.</summary>
    Author: string;
    ///  <summary>Source code of SWAG packet.</summary>
    ///  <remarks>Strictly speaking this is the text of the packet since not
    ///  all packets are pure source code - some are text documents.</remarks>
    SourceCode: string;
    ///  <summary>Flag that indicates if SWAG packet is a text document (True)
    ///  or is Pascal source code (False).</summary>
    IsDocument: Boolean;
  end;


implementation

end.

