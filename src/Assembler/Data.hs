-- | Holds all the data types relevant to the assembler
module Assembler.Data where

import Machine.Data(Operation(..))

-- | Holds state data about the current instruction being assembled.
data Metadata = Metadata {
    labelTable :: M.Map String Integer, -- ^ 'M.Map' holding all defined labels and
                                        --   their respective values.
    wordOffset :: Integer               -- ^ Current instruction offset (i.e. how
                                        --   many instructions have already been
                                        --   assembled).
} deriving (Eq, Show)

-- | An instruction is what the assembler uses as an intermediate language. Each
--   supported opcode is converted to an 'Instruction'.
data Instruction    = MachineOp Operation                               -- ^ Naturally, every machine
                                                                        --   opcode needs to be represented.
                                                                        --   See 'Operation'.
                    | BeqL      Source      Tource  Label   Position    -- ^ A form of 'Beq' that accepts
                                                                        --   labels instead of offsets.
                                                                        --   During generation, the offset
                                                                        --   will be calculated from the
                                                                        --   'Position' (which is the word offset
                                                                        --   of this instruction) and the offset
                                                                        --   of the label definition.
                    | BneL      Source      Tource  Label   Position    -- ^ Like 'BeqL' for 'Bne'.
                    deriving (Eq, Show)

type Position = Integer
