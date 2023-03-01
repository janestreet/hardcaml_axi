# AXI interfaces

Hardcaml_axi defines a the types and modules for working with
AXI interfaces.

- The [`Stream` module](./src/stream_intf.ml) defines the type for AXI4-streams.
  This `Datapath_register` submodule contains functions for constructing skid buffers.

- The [`Lite` module](./src/lite_intf.ml) defines master and slave interfaces for
  working with the AX4-Lite protocol. The `Register_bank` submodule contains
  functions for construction register interfaces using AXI4-Lite for I/O.

Some other useful modules available in this library includes:

- The [`Internal_bus` module](./src/internal_bus_intf.ml) defines a simplified
  protocol for representing read/write requests
- The [`Slave_statemachine` module](./src/slave_statemachine.mli) contains
  components for converting AXI4-Lite into the `Internal_bus` protocol.
- The [`Address_space_decoder` module](./src/address_space_decoder.mli) contains
  utilities for decoding address spaces.

For more information about AXI interfaces, please refer to
Xilinx UG1037.
